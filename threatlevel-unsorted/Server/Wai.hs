module Server.Wai where

import Common.Prelude
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Lens qualified as LL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString as BS
import System.Environment
import System.IO.Unsafe
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Network.Wai as Wai
import Network.HTTP.Types.URI as Wai
import Network.HTTP.Types
import Web.Cookie

-- * Request

-- ** Headers

getDomain :: Request -> Maybe BS.ByteString
getDomain = lookup "Host" . requestHeaders

reqCookie :: Request -> Maybe BS.ByteString
reqCookie = lookup "Cookie" . requestHeaders

hasCookie :: TS.Text -> TS.Text -> Request -> Bool
hasCookie k v = getCookie k ^ maybe False (v ==)

getCookie :: TS.Text -> Request -> Maybe TS.Text
getCookie k = requestCookies >=> lookup k

requestCookies :: Request -> Maybe CookiesText
requestCookies = requestHeaders
  ^ lookup hCookie
  ^ fmap parseCookiesText

-- ** Params

queryText :: Request -> QueryText
queryText = queryString ^ queryToQueryText

queryParam' :: TS.Text -> Request -> Maybe (Maybe TS.Text)
queryParam' name req = lookup name $ queryText req

queryParam :: TS.Text -> Request -> Maybe TS.Text
queryParam name req = join $ queryParam' name req

hasParam :: TS.Text -> Request -> Bool
hasParam name req = isJust $ lookup name $ queryText req

-- ** Body

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = BL.fromChunks <$> loop
   where
      loop = getRequestBodyChunk req >>= re
      re chunk = BS.null chunk ? return [""] $ (chunk:) <$> loop

-- | Get @request@ POST body as QueryText
postKvs :: Request -> IO QueryText
postKvs request = do
  bs <- getRequestBody request
  return $ parseQueryText $ BL.toStrict bs

-- ** TLS settings with certificate

tlsSettingsEnv :: String -> String -> IO (Maybe Warp.TLSSettings)
tlsSettingsEnv cert key = do
  certPath <- lookupEnv cert
  keyPath <- lookupEnv key
  return $ Warp.tlsSettings <$> certPath <*> keyPath

tlsSettingsUnsafe :: String -> String -> Maybe Warp.TLSSettings
tlsSettingsUnsafe cert key = unsafePerformIO $ tlsSettingsEnv cert key

runWarp :: Maybe Warp.TLSSettings -> Warp.Settings -> Wai.Application -> IO ()
runWarp maybeTls = maybe Warp.runSettings Warp.runTLS maybeTls

-- * Form submission

formSubmission :: Wai.Request -> IO [(BL.ByteString, Maybe BL.ByteString)]
formSubmission req = do
  lb <- Wai.strictRequestBody req
  let pairs = BL8.split '&' lb :: [BL.ByteString] -- queryString
      f (k, v) = case v of
        "" -> (k, Nothing)
        _ -> (k, Just $ BL8.tail v)
      g = strict %~ Wai.urlDecode True
      decode (k, v) = (g k, g <$> v)
  return $ map (decode . f . BL8.break (== '=')) pairs

textFormSubmission :: Wai.Request -> IO [(TL.Text, Maybe TL.Text)]
textFormSubmission req = do
  formSubmission req <&> map (f *** fmap f)
  where
    f = view LL.utf8
