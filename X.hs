module X
  ( module X
  , module Export
  ) where

import HTML as Export hiding (
  -- redefined
  href, src,
  -- used in CSS
  em, font, content, Value,
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

import Web.Cookie (parseCookiesText)
import Network.Wai as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.Mime as Mime

import qualified HTTP.Header as Hdr
import qualified HTTP.Response as HR

import Prelude2 as P
import Data.Default
import Render
import JS
import qualified JS.Render

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

-- todo: make this a lens for headers on AnyResponse
withHeaders f req = case req of
  WR.Response s h ar -> WR.Response s (f h) ar

deleteCookie :: TS.Text -> WR.Response -> WR.Response
deleteCookie key = withHeaders (Hdr.delC (TL.fromStrict key) :)

setCookie :: TS.Text -> TS.Text -> WR.Response -> WR.Response
setCookie k v = withHeaders (setCookie (TL.fromStrict v))
  where
    setCookie :: TL.Text -> [Hdr.Header] -> [Hdr.Header]
    setCookie val = (Hdr.cookie' (TL.fromStrict k) val Nothing (Just []) Nothing :)

hasCookie :: TS.Text -> TS.Text -> Wai.Request -> P.Bool
hasCookie k v req = req
  & Wai.requestHeaders
  ^ lookup Wai.hCookie
  ^ fmap parseCookiesText
  ^ maybe False (lookup k ^ maybe False (v ==))

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

-- * Serve static files as if in folder paths

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
staticDiskSubtree notFound (fp :: FilePath) = do
  return $ \req -> do
    e <- asks (view WE.dynPath) <&> sanitizePath
    liftIO $ print e
    e & either
      (\err -> do
          liftIO $ print err
          return notFound
      )
      (\subPath -> WR.diskFile (fp <> "/" <> subPath))
  where
    sanitizePath :: [TS.Text] -> Either P.String P.String
    sanitizePath parts = if any (== "..") parts
      then Left "Not allowed to go up"
      else Right (TS.unpack $ TS.intercalate "/" parts)
