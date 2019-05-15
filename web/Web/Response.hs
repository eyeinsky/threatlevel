module Web.Response where

import Pr
import Language.Haskell.TH
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import Network.Mime as Mime
import qualified Network.HTTP.Types as WT
import Data.FileEmbed
import qualified Data.Aeson as Aeson

import qualified Data.Text.Strict.Lens as LS
import qualified Data.Text.Lazy.Lens as LL


import qualified HTTP.Header as Hdr
import HTTP.Response hiding (redirect, JSON, Response)
import qualified JS
import qualified JS.Syntax
import qualified HTML
import URL
import Render
import HTTP.Common (ToPayload(..))

-- * Url path

renderURL :: URL -> TL.Text
renderURL url = toPayload url

toTextList :: URL -> [Segment]
toTextList url = domain : url^.URL.segments
   where
     domain = TL.toStrict $ toPayload (url^.proto) <> "://" <> toPayload (url^.authority)

data AnyResponse where
  HtmlDocument :: HTML.Document -> AnyResponse
  JS :: JS.Syntax.Conf -> JS.Expr a -> AnyResponse
  JSON :: Aeson.ToJSON a => a -> AnyResponse
  Raw :: BL.ByteString -> AnyResponse

instance Show AnyResponse where
  show _ = "AnyResponse"

declareFields [d|
  data Response = Response
    { responseCode :: WT.Status
    , responseHeaders :: [Hdr.Header]
    , responseBody :: AnyResponse
    } deriving Show
  |]

instance ToRaw Response where
  toRaw r'@ (Response status origHeaders anyResponse)
    = httpResponse status (origHeaders <> headers) bl
    where
      (headers, bl) = case anyResponse of
        HtmlDocument (HTML.Document h b) -> ([Hdr.utf8text "html"], tl^.re utf8)
          where
            html' = HTML.html (HTML.head h >> b)
            tl = "<!DOCTYPE html>" <> render () html'
        JS conf code -> ([Hdr.javascript], render conf code^.re LL.utf8)
        JSON a -> ([Hdr.json], Aeson.encode a)
        Raw b -> ([], b)

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
error code message = rawText code [Hdr.hdr Hdr.ContentType "text/plain"] message

redirect :: URL -> Response
redirect url = redirectRaw $ renderURL url

redirectRaw :: TL.Text -> Response
redirectRaw url = rawText (toEnum 303) [Hdr.hdr Hdr.Location url] ""

redirect' code url = rawText (toEnum code) headers ""
  where headers = [Hdr.hdr Hdr.Location $ renderURL url]

redirectRaw' code url = rawText (toEnum code) [Hdr.hdr Hdr.Location url] ""

-- ** Raw

rawBl :: WT.Status -> [Hdr.Header] -> BL.ByteString -> Response
rawBl status headers bl = Response status headers $ Raw bl

rawBS :: WT.Status -> [Hdr.Header] -> BS.ByteString -> Response
rawBS status headers bl = Response status headers $ Raw (bl^.from strict)

rawText :: WT.Status -> [Hdr.Header] -> Text -> Response
rawText status headers text = Response status headers $ Raw (text^.re LL.utf8)

raw :: Text -> Text -> Response
raw headers text = Response (toEnum 200) [Hdr.hdr Hdr.ContentType headers] $ Raw (text^.re LL.utf8)

-- ** File

diskFile :: MonadIO m => FilePath -> m Response
diskFile path = do
  bytes <- liftIO $ BL.readFile path
  let ct = path^.packed.to Mime.defaultMimeLookup.LS.utf8.from strict :: TL.Text
  return $ rawBl (toEnum 200) [Hdr.hdr Hdr.ContentType $ ct] bytes

-- | Embeds a file from path into binary, resulting file's type will
-- be Response.
embeddedFile :: TL.Text -> ExpQ
embeddedFile path = let
    filePath = path^.from packed :: FilePath
    ct = path^.from lazy.to Mime.defaultMimeLookup.LS.utf8.from packed & stringE
  in [| let header = Hdr.hdr Hdr.ContentType $ct
        in rawBS 200 [header] $(embedFile filePath)
           :: Response|]
