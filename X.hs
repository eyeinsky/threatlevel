module X
  ( module X
  , module Export
  ) where

import HTML as Export hiding (
  -- redefined
  href, src,
  -- used in CSS
  em, font, content, Value,
  -- used in HTTP
  header
  )
import CSS as Export hiding (
  -- generic
  filter, all, transform
  )
import Web.Monad as Export
import DOM.Event as Export


import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Lens as LL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

import Control.Monad.IO.Class
import Control.Monad.Reader

import Web.Cookie as Wai
import Network.Wai as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.Mime as Mime

import System.Process as IO
import System.IO as IO
import Language.Haskell.TH

import qualified HTTP.Header as Hdr
import qualified HTTP.Response as HR

import Prelude2 as P
import Data.Default
import Render
import JS hiding (String)
import qualified JS.Render

import qualified HTTP.Header as HH
import qualified URL
import qualified HTML
import qualified DOM
import qualified Web as W
import qualified Web.Monad as WM
import qualified Web.Response as WR
import qualified Web.Endpoint as WE


-- * DOM.Event

-- | Create inline on-event attribute
on :: DOM.Event e => e -> Expr a -> Attribute
on event handler = Custom (DOM.toOn event) (render def $ call1 handler $ ex "event")
  where
    -- JS.Syntax.EName (JS.Syntax.Name handler') = handler
    -- ^ todo: find a generic way to get the name, even for literal
    -- expressinos.

post url = DOM.xhrRaw "POST" (ulit $ WR.renderURL url)
get url = DOM.xhrRaw "GET" (ulit $ WR.renderURL url)

-- * HTML

href :: URL.URL -> Attribute
href url = HTML.href (WR.renderURL url)

-- * HTTP.Response

deleteCookie :: TS.Text -> WR.Response -> WR.Response
deleteCookie key = WR.headers %~ (Hdr.delC (TL.fromStrict key) :)

setCookie :: TS.Text -> TS.Text -> WR.Response -> WR.Response
setCookie k v = WR.headers %~ (setCookie (TL.fromStrict v))
  where
    setCookie :: TL.Text -> [Hdr.Header] -> [Hdr.Header]
    setCookie val = (Hdr.cookie' (TL.fromStrict k) val Nothing (Just []) Nothing :)

hasCookie :: TS.Text -> TS.Text -> Wai.Request -> P.Bool
hasCookie k v = getCookie k ^ maybe False (v ==)

getCookie :: TS.Text -> Wai.Request -> Maybe TS.Text
getCookie k = requestCookies >=> lookup k

requestCookies :: Wai.Request -> Maybe Wai.CookiesText
requestCookies = Wai.requestHeaders
  ^ lookup Wai.hCookie
  ^ fmap Wai.parseCookiesText

-- * HTML

includeCss :: URL.URL -> Html
includeCss url = link ! rel "stylesheet" ! type_ "text/css" ! href url $ pure ()

includeJs :: URL.URL -> Html
includeJs url = script ! src url $ "" ! Custom "defer" "true"

src :: URL.URL -> Attribute
src url = HTML.src (WE.renderURL url)

-- * Endpoint

exec jsm = do
  browser <- asks (view W.browser)
  stWeb <- WM.getState
  let
    stJs = stWeb^.WM.jsState
    c = JS.Render.Indent 2
    ((_, w), _) = JS.runM (JS.Conf browser True c) stJs jsm
  return $ WR.js c $ call0 $ Par $ AnonFunc Nothing [] w

-- * Serving static assets

-- | Serve source-embedded files by their paths. Note that for dev
-- purposes the re-embedding of files might take too much time.
statics' (pairs :: [(FilePath, BS.ByteString)]) = forM pairs $ \(path, bs) -> let
  mime = path^.packed.to Mime.defaultMimeLookup.from strict & TL.decodeUtf8
  headers = [HR.contentType mime]
  response = WR.rawBl (toEnum 200) headers (bs^.from strict)
  path' = TS.pack path
  in (path,) <$> (WE.pin path' $ WE.staticResponse response)

-- | Generate endpoints for source-embedded files and return the html
-- to include them.
includes (pairs :: [(FilePath, BS.ByteString)]) = statics' pairs <&> map f ^ sequence_
  where
    f :: (FilePath, URL.URL) -> Html
    f (path, url) = case P.split "." path^.reversed.ix 0 of
      "css" -> includeCss url
      "js" -> includeJs url
      _ -> pure ()

-- | Serve the subtree at fp from disk. The url is generated, the rest
-- needs to match file's path in the. TODO: resolve ".." in path and
-- error out if path goes outside of the served subtree. And check the
-- standard of if .. is even allowed in url paths.
staticDiskSubtree' mod notFound (fp :: FilePath) = do
  return $ \req -> do
    e <- asks (view WE.dynPath) <&> sanitizePath
    e & either
      (\err -> do
          liftIO $ print err
          return notFound
      )
      (\subPath -> mod <$> WR.diskFile (fp <> "/" <> subPath))
  where
    sanitizePath :: [TS.Text] -> Either P.String P.String
    sanitizePath parts = if any (== "..") parts
      then Left "Not allowed to go up"
      else Right (TS.unpack $ TS.intercalate "/" parts)

-- | Serve entire path from under created url
staticDiskSubtree notFound path = staticDiskSubtree' id notFound path

-- | Serve files from filesystem path using a content adressable hash
assets notFound path = do
  hashPin path $ staticDiskSubtree' h notFound path
  where
    forever = HH.header HH.CacheControl "max-age=365000000, public, immutable"
    h = WR.headers <>~ [forever]

    hashPin path what = do
      hash <- liftIO (folderHash path) <&> TS.pack
      liftIO $ print (path, hash)
      WE.pin hash what

folderHash :: String -> IO [Char]
folderHash path = do
  (i,o,e,h) <- IO.runInteractiveCommand cmd
  IO.hGetContents o <&> P.take 40
  where
    cmd = "tar cf - '" <> path <> "' | shasum | cut -d ' ' -f 1"
    -- todo: better path escaping

folderHashTH :: FilePath -> ExpQ
folderHashTH path = runIO (folderHash path) >>= stringE
