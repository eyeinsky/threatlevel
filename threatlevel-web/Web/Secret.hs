module Web.Secret where

import X.Prelude
import Data.Char

import Crypto.Cipher.AES
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), IV, makeIV)
import Crypto.Error (CryptoFailable(..), CryptoError(..))

import qualified Crypto.Random.Types as CRT

import Data.ByteArray (ByteArray)
import Data.ByteString qualified as BS

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as TE

import X
import JS
import CSS qualified
import Web.DSL
import HTML hiding (id)

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64URL

-- * API

-- | With existing key, create a secret site
secretSite :: String -> Web Html -> (BS.ByteString, Web Html)
secretSite code site = secretSitePrim key iv site
  where
    (key, iv) = fromBase64url code

-- | Generate a key and encrypt the site with it
secretSiteIO :: Web Html -> IO (BS.ByteString, Web Html)
secretSiteIO site = do
  (key, iv) <- genKeyIv
  return $ secretSitePrim key iv site

secretSitePrim :: BS.ByteString -> BS.ByteString -> Web Html -> (BS.ByteString, Web Html)
secretSitePrim key iv site = (key <> iv, wrapSite key' iv' site)
  where
    key' = Key key :: Key AES256 BS.ByteString
    iv' = fromJust $ makeIV iv

-- | Wrap the encrypted page with a page that on load time decrypts it
-- with the key from URL hash
wrapSite :: Key AES256 BS.ByteString -> IV AES256 -> Web Html -> Web Html
wrapSite key iv secretSite = site
  where
    secret = base64text $ encryptSite key iv secretSite
    site = do
      rule ("body" :: TagName) $ do
        width $ prc 100
        height $ vh 100
        display "flex"
        flexFlow "column nowrap"
      encrypted <- css $ do
        width $ prc 60
        wordBreak "break-all"
        fontFamily "monospace"
        margin "auto"
        hover $ do
          backgroundColor "yellow"
      replace <- newf documentReplace
      onload <- newf $ do
        hash <- const $ (ex "window.location.hash")
        ifonly hash $ do
          hex :: Expr String <- const $ toString $ parseBase64 $ base64urlToBase64 $ call1 (hash !. "slice") 1
          key <- const $ parseHex $ slice 0 64 hex
          iv <- const $ parseHex $ slice 64 Undefined hex
          bare $ call1 replace (decrypt' (querySelector encrypted document !. "innerHTML") key iv)
      bare $ addEventListener (Cast window) Load onload
      window !. "onhashchange" JS..= onload
      return $ do
        cryptoJs
        div ! encrypted $ text secret

encryptSite :: Key AES256 BS.ByteString -> IV AES256 -> Web Html -> BS.ByteString
encryptSite key iv site = em
  where
    htmlText = render' $ runStatic site :: Text
    bs = htmlText^.re TE.utf8.from lazy :: BS.ByteString
    padded = bs <> BS.replicate (16 - (BS.length bs `mod` 16)) (toEnum $ fromEnum $ ord ' ')
    Right em = encrypt key iv padded :: Either CryptoError BS.ByteString

-- * CryptoJS

cryptoJs :: Html
cryptoJs = script ! Custom "src" "https://cdnjs.cloudflare.com/ajax/libs/crypto-js/3.1.9-1/crypto-js.js" $ text ""

data WordArray

parseBase64 :: Expr String -> Expr WordArray
parseBase64 t = call1 (ex "CryptoJS.enc.Base64.parse") t

toString' v = call1 (v !. "toString") (ex "CryptoJS.enc.Utf8")
decrypt' s key iv = dec'
  where
    obj' = lit $ obj iv
    dec = call (ex "CryptoJS.AES.decrypt") [s, key, obj']
    dec' = toString' dec

parseHex :: Expr String -> Expr WordArray
parseHex = call1 (ex "CryptoJS.enc.Hex.parse")

-- * JS

-- htmlEscape unescaped = return_ $ foldl f unescaped htmlTextEscapeMap
--   where
--     f b (u, e) = replace ([u]^.packed.to g) (lit e) b

-- htmlUnescape escaped = return_ $ foldl f escaped htmlTextEscapeMap
--   where
--     f b (u, e) = replace (g $ e^.from lazy) (lit [u]) b

replace a b str = call (str !. "replace") [a, b]
padEnd a b str = call (str !. "padEnd") [a, b]

type Base64 = String
type Base64Url = String

base64urlToBase64 :: Expr Base64Url -> Expr Base64
base64urlToBase64 = replace (g "-") "+" ^ replace (g "_") "/" ^ replace (regex "(=)+$" "") ""

base64toBase64url :: Expr Base64 -> Expr Base64Url
base64toBase64url str = str & replace (g "+") "-" ^ replace (g "/") "_" ^ padEnd (4 - ((str !. "length") % 4)) "="

g :: TS.Text -> Expr RegExp
g pat = regex pat "g"

-- ** DOM

documentReplace :: JS m => Expr a -> m ()
documentReplace unescapedContent = do
  newDoc <- const $ call (document !. "open") ["text/html", "replace"]
  bare $ call1 (newDoc !. "write") unescapedContent
  bare $ call0 (newDoc !. "close")

base64text :: BS.ByteString -> TL.Text
base64text bs = bs^.to B64.encode.lazy.TE.utf8

-- * Server side crypto

data Key c a where
  Key :: (BlockCipher c, ByteArray a) => a -> Key c a

fromBase64 :: String -> (BS.ByteString, BS.ByteString)
fromBase64 str = BS.splitAt 32 bs
  where
    bs = either explode id $ B64.decode $ urlDecode True $ fromString str :: BS.ByteString
    explode = todoMsg "fromBase64: can't decode"

fromBase64url :: String -> (BS.ByteString, BS.ByteString)
fromBase64url str = BS.splitAt 32 bs
  where
    bs = either explode id $ B64URL.decode $ urlDecode True $ fromString str :: BS.ByteString
    explode = todoMsg "fromBase64url: can't decode"

genKeyIv :: CRT.MonadRandom m => m (BS.ByteString, BS.ByteString)
genKeyIv = do
  keyBs <- CRT.getRandomBytes 32 -- 32 byte = 256 bits key
  iv <- genRandomIV (undefined :: AES256)
  return (keyBs, iv)

-- | Generate a random initialization vector for a given block cipher
genRandomIV :: forall m c. (CRT.MonadRandom m, BlockCipher c) => c -> m BS.ByteString
genRandomIV _ = CRT.getRandomBytes $ blockSize (undefined :: c)

-- | Initialize a block cipher
initCipher :: (BlockCipher c, ByteArray a) => Key c a -> Either CryptoError c
initCipher (Key k) = case cipherInit k of
  CryptoFailed e -> Left e
  CryptoPassed a -> Right a

-- encrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
-- encrypt secretKey initIV msg =
--   case initCipher secretKey of
--     Left e -> Left e
--     Right c -> Right $ ecbEncrypt c msg
-- obj _ = [("mode", ex "CryptoJS.mode.ECB"), ("padding", ex "CryptoJS.pad.NoPadding")]

encrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
encrypt secretKey initIV msg =
  case initCipher secretKey of
    Left e -> Left e
    Right c -> Right $ cbcEncrypt c initIV msg

obj :: Expr WordArray -> [(String, Expr String)]
obj iv = [("iv", Cast iv), ("mode", ex "CryptoJS.mode.CBC"), ("padding", ex "CryptoJS.pad.NoPadding")]

decrypt :: (BlockCipher c, ByteArray a) => Key c a -> IV c -> a -> Either CryptoError a
decrypt = encrypt

runStatic :: Web Html -> HTML.Document
runStatic wm = HTML.Document head' body'
  where
    (body' :: Html, _, (css', js)) = Web.DSL.runFresh wm
    css = CSS.wrapW (CSS.selFrom CSS.Any) css'
    head' = do
      HTML.style $ raw $ render (CSS.Pretty 2) css
      HTML.script $ raw $ render (JS.Indent 2) js
      HTML.favicon "data:;base64,iVBORw0KGgo="
      meta
        ! httpEquiv "Content-Type"
        ! HTML.content "text/html; charset=utf-8" $ pure ()
