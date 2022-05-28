module Server.Response where

import qualified Prelude as P
import X.Prelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types as WT
import qualified Network.Wai as Wai
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Lens as LL

import qualified HTTP.Header as Hdr
import HTTP.Response
import qualified JS
import qualified JS.Syntax
import qualified HTML
import Render
import URL

import qualified Network.WebSockets as WS


data AnyResponse where
  HtmlDocument :: HTML.Document -> AnyResponse
  JS :: JS.Syntax.Conf -> JS.M r a -> AnyResponse
  JSON :: Aeson.ToJSON a => a -> AnyResponse
  Raw :: BL.ByteString -> AnyResponse

instance Show AnyResponse where
  show _ = "AnyResponse"

declareFields [d|
  data Response
    = Response
      { responseCode :: WT.Status
      , responseHeaders :: [Hdr.Header]
      , responseBody :: AnyResponse
      }
    | WebSocket WS.ServerApp
    | File WT.Status [Hdr.Header] FilePath (Maybe Wai.FilePart)
  |]

instance ToRaw Response where
  toRaw r = case r of
    Response status origHeaders anyResponse -> httpResponse status (origHeaders <> headers) bl
      where
        (headers, bl) = case anyResponse of
          HtmlDocument (HTML.Document h b) -> ([Hdr.utf8text "html"], tl^.re LL.utf8)
            where
              html' = HTML.html (HTML.head h >> b)
              tl = "<!DOCTYPE html>" <> render () html'
          JS conf mcode -> let
            ((_, code),_) = JS.runEmpty conf mcode
            in ([Hdr.javascript], render conf code^.re LL.utf8)
          JSON a -> ([Hdr.json], Aeson.encode a)
          Raw b -> ([], b)

    WebSocket _ -> P.error "ToRaw: Response(WebSocket) can't be converted to Raw"
    -- ^ fix: Figure out a better solution

    File status headers path maybePartInfo -> Wai.responseFile status (map Hdr.toWai headers) path maybePartInfo

-- * Helpers

-- | Successful response with empty headers
resp200 :: AnyResponse -> Response
resp200 = Response (toEnum 200) []

htmlDoc head body = resp200 $ HtmlDocument (HTML.Document head body)
page html = resp200 $ HtmlDocument $ HTML.docBody $ html

renderedPage = resp200 . Raw

text :: TL.Text -> Response
text text = Response (toEnum 200) hs $ Raw (text^.re LL.utf8)
  where hs = [Hdr.utf8text "plain"]

json a = resp200 $ JSON a

error :: WT.Status -> TL.Text -> Response
error code message = rawText code [Hdr.header Hdr.ContentType "text/plain"] message

redirect :: URL -> Response
redirect url = redirectRaw $ renderURL url

redirectRaw :: TL.Text -> Response
redirectRaw url = rawText (toEnum 303) [Hdr.header Hdr.Location url] ""

redirect' code url = rawText (toEnum code) headers ""
  where headers = [Hdr.header Hdr.Location $ renderURL url]

redirectRaw' code url = rawText (toEnum code) [Hdr.header Hdr.Location url] ""

noRobots :: Response
noRobots = raw "text/plain"
  [unindent|
    User-agent: *
    Disallow: /
    |]

-- ** Raw

rawBl :: WT.Status -> [Hdr.Header] -> BL.ByteString -> Response
rawBl status headers bl = Response status headers $ Raw bl

rawBS :: WT.Status -> [Hdr.Header] -> BS.ByteString -> Response
rawBS status headers bl = Response status headers $ Raw (bl^.from strict)

rawText :: WT.Status -> [Hdr.Header] -> Text -> Response
rawText status headers text = Response status headers $ Raw (text^.re LL.utf8)

raw :: Text -> Text -> Response
raw headers text = Response (toEnum 200) [Hdr.header Hdr.ContentType headers] $ Raw (text^.re LL.utf8)
