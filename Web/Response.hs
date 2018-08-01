module Web.Response where

import Pr
import Language.Haskell.TH
import qualified Data.ByteString.Lazy as BL
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
import HTTP.Response hiding (redirect, JSON)
import qualified JS
import qualified JS.Render
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
  JS :: JS.Render.Conf -> JS.Code a -> AnyResponse
  JSON :: Aeson.ToJSON a => a -> AnyResponse
  Raw :: WT.Status -> [Hdr.Header] -> BL.ByteString -> AnyResponse

-- inlineFile ct = return $ R

instance ToResponse AnyResponse where
  toResponse ar = case ar of
     HtmlDocument a -> toResponse a
     JS conf code -> Response (toEnum 200) [javascript] (render conf code^.re LL.utf8)
     JSON a -> Response (toEnum 200) [HTTP.Response.json] (Aeson.encode a)
     Raw a h b -> Response a h b

page html = HtmlDocument $ HTML.docBody $ html
renderedPage = Raw (toEnum 200) [utf8textHdr "html"]
text text = Raw (toEnum 200) [utf8textHdr "plain"] (text^.re LL.utf8)
js conf code = JS conf code
json a = JSON a
redirect :: URL -> AnyResponse
redirect url = redirectRaw $ renderURL url

file' :: MonadIO m => FilePath -> m AnyResponse
file' path = do
  bytes <- liftIO $ BL.readFile path
  let ct = path^.packed.to Mime.defaultMimeLookup.LS.utf8.from strict :: TL.Text
  return $ Raw (toEnum 200) [Hdr.hdr Hdr.ContentType $ ct] bytes

redirectRaw :: TL.Text -> AnyResponse
redirectRaw url = Raw (toEnum 303) [Hdr.hdr Hdr.Location url] ""

file :: TL.Text -> ExpQ
file path = let
    pathS = path^.from packed :: FilePath
    ct = path^.from lazy.to Mime.defaultMimeLookup.LS.utf8.from packed & stringE
  in [| Raw 200 [Hdr.hdr Hdr.ContentType $ct] ($(embedFile pathS)^.from strict) |]
