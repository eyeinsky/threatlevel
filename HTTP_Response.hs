module HTTP_Response where

import Prelude2

import qualified Text.Blaze.Html5            as E
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

-- Wai/Warp conversions
import Network.Wai ( Response, responseBuilder, responseFile
                   , Request
                   , pathInfo, queryString, requestHeaders, requestBody
                   , requestMethod
                   )
import Network.Wai.Internal (Response(ResponseBuilder))
import Network.HTTP.Types (
     ok200, hContentType, hCacheControl, hCookie, queryToQueryText, parseQuery, RequestHeaders, methodGet, urlEncode
   , hServer, status303, status404, status503, QueryText)
import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)

import Data.Aeson as JSON

import qualified HTTP_Header as Hdr


-- * Response

-- The high-level response type

newtype Resp = Resp { unResp :: ([Hdr.Header], RespAction) }


-- stuff that differs
data RespAction where
   Html     :: E.Html -> E.Html -> RespAction
   JSON     :: ToJSON a => a -> RespAction 
   Redirect :: T.Text -> RespAction

addHeader :: Hdr.Header -> Resp -> Resp
addHeader c (Resp (hs, resp)) = Resp (c : hs, resp)


-- ** Shorthands

utf8ct what ra = Resp (common, ra)
   where
   common :: [ Hdr.Header ]
   common = [ Hdr.Header (Hdr.ContentType, "text/"<>what<>"; charset=UTF-8") ]

utf8textHdr what = Hdr.Header (Hdr.ContentType, "text/"<>what<>"; charset=UTF-8")

addHead a (Resp (hs, ra)) = Resp (hs, ra')
   where ra' = case ra of Html h b -> Html (h>>a) b; _ -> ra

prependBody a (Resp (hs, ra)) = Resp (hs, ra')
   where ra' = case ra of Html h b -> Html h (a>>b); _ -> ra
postpendBody a (Resp (hs, ra)) = Resp (hs, ra')
   where ra' = case ra of Html h b -> Html h (b>>a); _ -> ra


-- * Conversion to Wai/Warp

toWai (Resp (hs, ra)) = waiAddHeaders (map Hdr.cc hs) $ case ra of
   Html hh hb -> waiBs [ utf8textHdr "html" ] . renderHtml
            $ E.docType >> E.head hh >> E.body hb
   JSON json    -> waiBs [ jsh ] $ encode json
   Redirect url -> waiRedir $ url
   where jsh = Hdr.Header (Hdr.ContentType, "application/json; charset=UTF-8")


waiBs :: [ Hdr.Header ] -> LBS.ByteString -> Response
waiBs hs bs = responseBuilder ok200 hs' bs'
   where hs' = map Hdr.cc hs
         bs' = fromLazyByteString bs

waiAddHeaders hs (ResponseBuilder st hdrs builder) =
   ResponseBuilder st (hs <> hdrs) builder

waiRedir :: T.Text -> Response
waiRedir url = responseBuilder status303 (("Location", url') : []) (fromLazyByteString $ "")
   where url' = TE.encodeUtf8 url

waiSendFile path = responseFile ok200 [htmlUtf8 path False] (T.unpack path) Nothing
htmlUtf8 fn bool = u -- (hContentType, Mime.defaultMimeLookup fn <> (bool ? "; charset=UTF-8" $ ""))

{-
sendEmbed assoc path = lookup path assoc
  & maybe
      u -- (notFoundPage tee)
      (responseBuilder ok200 [(hContentType, Mime.defaultMimeLookup fn)]
      . BB.fromByteString)
   where fn = T.reverse . T.takeWhile ('/' /=) . T.reverse $ path 

err msg = maybe (returnHtmlPage msg)

sendAttachment :: T.Text -> T.Text -> Response
sendAttachment name path = responseFile ok200 [dispDownload name] (T.unpack path) Nothing

-- sendFileAge maxAge path = responseFile ok200 [htmlUtf8 path False, mkAge maxAge] (T.unpack tee) Nothing

dispDownload fn = ("Content-Disposition", "attachment; filename=\""<>TE.encodeUtf8 fn<>"\"")
-}

