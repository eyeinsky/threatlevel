module HTTP_Response where

import Prelude2

import qualified Text.Blaze.Html5            as E
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified HTTP_Header as Hdr

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




-- * Response

newtype Resp = Resp { unResp :: ([Hdr.Header], RespAction) }


-- stuff that differs
data RespAction
   = Html     E.Html E.Html
   | JSON     E.Html
   | Redirect T.Text


addHeader :: Hdr.Header -> Resp -> Resp
addHeader c (Resp (hs, resp)) = Resp (c : hs, resp)


-- ** Shorthands

utf8Html ra = Resp (common, ra)
   where
   common :: [ Hdr.Header ]
   common = [ Hdr.Header (Hdr.ContentType, "text/html; charset=UTF-8") ]


addHead a (Resp (hs, ra)) = Resp (hs, ra')
   where ra' = case ra of Html h b -> Html (h>>a) b; _ -> ra

prependBody a (Resp (hs, ra)) = Resp (hs, ra')
   where ra' = case ra of Html h b -> Html h (a>>b); _ -> ra
postpendBody a (Resp (hs, ra)) = Resp (hs, ra')
   where ra' = case ra of Html h b -> Html h (b>>a); _ -> ra


-- * Conversion to Wai/Warp

toWai (Resp (hs, ra)) = f $ case ra of
   Html     hh hb -> waiHtml $ E.docType >> E.head hh >> E.body hb
   JSON     hb    -> waiHtml $ hb
   Redirect url   -> waiRedir $ url
   where
      f (ResponseBuilder st hdrs builder) =
         ResponseBuilder st (map Hdr.cc hs <> hdrs) builder

waiHtml :: E.Html -> Response
waiHtml html = responseBuilder ok200 [] (fromLazyByteString . renderHtml $ html)

waiRedir :: T.Text -> Response
waiRedir url = responseBuilder status303 (("Location", url') : []) (fromLazyByteString $ "")
   where url' = TE.encodeUtf8 url

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

sendFile path = responseFile ok200 [htmlUtf8 path False] (T.unpack path) Nothing
-- sendFileAge maxAge path = responseFile ok200 [htmlUtf8 path False, mkAge maxAge] (T.unpack tee) Nothing

dispDownload fn = ("Content-Disposition", "attachment; filename=\""<>TE.encodeUtf8 fn<>"\"")
htmlUtf8 fn bool = (hContentType, Mime.defaultMimeLookup fn <> (bool ? "; charset=UTF-8" $ ""))
-}

