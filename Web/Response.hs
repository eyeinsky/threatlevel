module Web.Response where

import Pr
import Language.Haskell.TH
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TE
import Network.Mime as Mime
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

toTextList :: URL -> [TL.Text]
toTextList url = domain : map TL.fromStrict (url^.URL.path.URL.segments)
   where
     domain = toPayload (url^.proto) <> "://" <> toPayload (url^.authority)

data AnyResponse where
  HtmlDocument :: HTML.Document -> AnyResponse
  JS :: JS.Code a -> AnyResponse
  JSON :: Aeson.ToJSON a => a -> AnyResponse
  Raw :: Int -> [Hdr.Header] -> BL.ByteString -> AnyResponse

-- inlineFile ct = return $ R

instance ToResponse AnyResponse where
  toResponse ar = case ar of
     HtmlDocument a -> toResponse a
     JS code -> Response 200 [utf8textHdr "plain"] (render JS.Render.Minify code^.re LL.utf8)
     JSON a -> Response 200 [jh] (Aeson.encode a)
     Raw a h b -> Response a h b
    where jh = Hdr.Header (Hdr.ContentType, "application/json; charset=UTF-8")

page html = HtmlDocument $ HTML.docBody $ html
renderedPage = Raw 200 [utf8textHdr "html"]
text text = Raw 200 [utf8textHdr "plain"] (text^.re LL.utf8)
js code = JS code
json a = JSON a
redirect :: URL -> AnyResponse
redirect url = redirectRaw $ renderURL url

redirectRaw :: TL.Text -> AnyResponse
redirectRaw url = Raw 303 [Hdr.hdr Hdr.Location url] ""

file :: TL.Text -> ExpQ
file path = let
    pathS = path^.from packed :: FilePath
    ct = path^.from lazy.to Mime.defaultMimeLookup.LS.utf8.from packed & stringE
  in [| Raw 200 [Hdr.hdr Hdr.ContentType $ct] ($(embedFile pathS)^.from strict) |]
