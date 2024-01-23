module Server.Response where

import qualified Prelude as P
import Common.Prelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types as WT
import qualified Network.Wai as Wai
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Lens as LL
import qualified Data.Text.Strict.Lens as SL

import Network.HTTP.Types
import HTTP.Response
-- import qualified JS
-- import qualified JS.Syntax
import qualified HTML
import Render
import URL

import HTML (Html, Document(Document))

import qualified Network.WebSockets as WS


docBody = undefined

utf8text :: BS.ByteString -> Header
utf8text what = (hContentType, ("text/"<>what<>"; charset=UTF-8"))

ctJson :: Header
ctJson = (hContentType, "application/json; charset=UTF-8")


data AnyResponse where
  HtmlDocument :: Document -> AnyResponse
--  JS :: JS.Syntax.Conf -> JS.M r a -> AnyResponse
  JSON :: Aeson.ToJSON a => a -> AnyResponse
  Raw :: BL.ByteString -> AnyResponse

instance Show AnyResponse where
  show _ = "AnyResponse"

declareFields [d|
  data Response
    = Response
      { responseCode :: WT.Status
      , responseHeaders :: [Header]
      , responseBody :: AnyResponse
      }
    | WebSocket WS.ServerApp
    | File WT.Status [Header] FilePath (Maybe Wai.FilePart)
  |]

instance ToRaw Response where
  toRaw r = case r of
    Response status origHeaders anyResponse -> httpResponse status (origHeaders <> headers) bl
      where
        (headers, bl) = case anyResponse of
          HtmlDocument (Document html') -> ([utf8text "html"], tl^.re LL.utf8)
            where
              tl = "<!DOCTYPE html>" <> render () html'
          -- JS conf mcode -> let
          --   ((_, code),_) = JS.runEmpty conf mcode
          --   in ([Hdr.javascript], render conf code^.re LL.utf8)
          JSON a -> ([ctJson], Aeson.encode a)
          Raw b -> ([], b)

    WebSocket _ -> P.error "ToRaw: Response(WebSocket) can't be converted to Raw"
    -- ^ fix: Figure out a better solution

    File status headers path maybePartInfo -> Wai.responseFile status headers path maybePartInfo

-- * Helpers

-- | Successful response with empty headers
resp200 :: AnyResponse -> Response
resp200 = Response (toEnum 200) []

htmlDoc :: Html -> Html -> Response
htmlDoc head body = resp200 $ HtmlDocument (Document $ HTML.html $ head >> body)

page :: Html -> Response
page html = resp200 $ HtmlDocument $ Document html

renderedPage :: BL.ByteString -> Response
renderedPage = resp200 . Raw

text :: TL.Text -> Response
text text = Response (toEnum 200) hs $ Raw (text^.re LL.utf8)
  where hs = [utf8text "plain"]

json :: Aeson.ToJSON a => a -> Response
json a = resp200 $ JSON a

error :: WT.Status -> TL.Text -> Response
error code message = rawText code [(hContentType, "text/plain")] message

redirect :: URL -> Response
redirect url = redirectRaw $ renderURL url

redirectRaw :: TL.Text -> Response
redirectRaw url = rawText (toEnum 303) [th hLocation url] ""

redirect' :: Int -> URL -> Response
redirect' code url = rawText (toEnum code) [th hLocation $ renderURL url] ""

redirectRaw' :: Int -> BS.ByteString -> Response
redirectRaw' code url = rawText (toEnum code) [(hLocation, url)] ""

noRobots :: Response
noRobots = raw "text/plain"
  "User-agent: *\n\
  \Disallow: / \n"

-- ** Raw

rawBl :: WT.Status -> [Header] -> BL.ByteString -> Response
rawBl status headers bl = Response status headers $ Raw bl

rawBS :: WT.Status -> [Header] -> BS.ByteString -> Response
rawBS status headers bl = Response status headers $ Raw (bl^.from strict)

rawText :: WT.Status -> [Header] -> Text -> Response
rawText status headers text = Response status headers $ Raw (text^.re LL.utf8)

raw :: Text -> Text -> Response
raw header text = Response (toEnum 200) [th hContentType header] $ Raw (text^.re LL.utf8)

th :: HeaderName -> TL.Text -> Header
th name textValue = (name, textValue^.strict.re SL.utf8)
