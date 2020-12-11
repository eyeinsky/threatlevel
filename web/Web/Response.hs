module Web.Response where

import qualified Prelude as P
import X.Prelude
import Language.Haskell.TH
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import Network.Mime as Mime
import qualified Network.HTTP.Types as WT
import Data.FileEmbed
import qualified Data.Aeson as Aeson

import qualified Data.Text.Strict.Lens as LS
import qualified Data.Text.Lazy.Lens as LL

import qualified HTTP.Header as Hdr
import HTTP.Response
import qualified JS
import qualified JS.Syntax
import qualified HTML
import Render
import URL

import qualified Network.WebSockets as WS

-- * Url path

renderURL :: URL -> TL.Text
renderURL url = render' url

data AnyResponse where
  HtmlDocument :: HTML.Document -> AnyResponse
  JS :: JS.Syntax.Conf -> JS.Expr a -> AnyResponse
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
          JS conf code -> ([Hdr.javascript], render conf code^.re LL.utf8)
          JSON a -> ([Hdr.json], Aeson.encode a)
          Raw b -> ([], b)
    WebSocket _ -> P.error "ToRaw: Response(WebSocket) can't be converted to Raw"
    -- ^ fix: Figure out a better solution

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

js conf code = resp200 $ JS conf code
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

-- ** Raw

rawBl :: WT.Status -> [Hdr.Header] -> BL.ByteString -> Response
rawBl status headers bl = Response status headers $ Raw bl

rawBS :: WT.Status -> [Hdr.Header] -> BS.ByteString -> Response
rawBS status headers bl = Response status headers $ Raw (bl^.from strict)

rawText :: WT.Status -> [Hdr.Header] -> Text -> Response
rawText status headers text = Response status headers $ Raw (text^.re LL.utf8)

raw :: Text -> Text -> Response
raw headers text = Response (toEnum 200) [Hdr.header Hdr.ContentType headers] $ Raw (text^.re LL.utf8)

-- ** File

diskFile :: MonadIO m => FilePath -> m Response
diskFile path = do
  bytes <- liftIO $ BL.readFile path
  let ct = path^.LS.packed.to Mime.defaultMimeLookup.LS.utf8.from strict :: TL.Text
  return $ rawBl (toEnum 200) [Hdr.hdr Hdr.ContentType $ ct] bytes

-- | Embeds a file from path into binary, resulting file's type will
-- be Response.
embeddedFile :: TL.Text -> ExpQ
embeddedFile path = let
    filePath = path^.from packed :: FilePath
    ct = path^.from lazy.to Mime.defaultMimeLookup.LS.utf8.from LS.packed & stringE
  in [| let header = Hdr.hdr Hdr.ContentType $ct
        in rawBS 200 [header] $(embedFile filePath)
           :: Response|]
