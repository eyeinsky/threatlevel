module HTTP.Response where

import Pr hiding (Any)

import qualified Text.Blaze.Html5            as E
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Blaze.ByteString.Builder as BBB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS

-- Wai/Warp conversions
import Network.Wai ( responseBuilder, responseFile
                   , Request
                   , pathInfo, queryString, requestHeaders, requestBody
                   , requestMethod
                   )
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiI
import Network.HTTP.Types (
     ok200, hContentType, hCacheControl, hCookie, queryToQueryText, parseQuery, RequestHeaders, methodGet, urlEncode
   , hServer, status303, status404, status503, QueryText)
import qualified Network.HTTP.Types as WT
import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Network.Mime as Mime
import Data.FileEmbed


import Data.Aeson as JSON

import qualified HTTP.Header as Hdr
import           HTTP.Header (hdr)
import qualified HTTP.Header as H
import qualified HTTP.Header as Hdr


import HTTP.Common
import URL


-- * Opaque

type Raw = Wai.Response

class ToRaw a where
  toRaw :: a -> Raw

httpResponse :: Int -> [Hdr.Header] -> BL.ByteString -> Wai.Response
httpResponse code headers body
  = responseBuilder (waiCode code) headers' (BBB.fromLazyByteString body)
  where
    headers' = map Hdr.cc headers

waiCode code = case code of
  200 -> WT.status200
  404 -> WT.status404
  303 -> WT.status303
  _ -> todo


-- * Helpers

utf8textHdr what = Hdr.Header (Hdr.ContentType, "text/"<>what<>"; charset=UTF-8")
-- | Typed response
data RespAction where
   Html     :: E.Html -> E.Html -> RespAction
   JSON     :: ToJSON a => a -> RespAction
   Redirect :: T.Text -> RespAction
   DiskFile :: T.Text -> RespAction
   InlineFile :: Hdr.Header -> B.ByteString -> RespAction
   Any      :: Int -> [Hdr.Header] -> BL.ByteString -> RespAction

declareFields [d|
  -- The high-level response type
  data Resp = Resp
    { respStatus :: Int
    , respHeaders :: [Hdr.Header]
    , respContent :: RespAction
    }
  |]

toWai :: Resp -> Wai.Response
toWai = toRaw

toResp :: RespAction -> Resp
toResp ra = Resp 200 [] ra

addHeader :: Hdr.Header -> Resp -> Resp
addHeader c r = r & headers %~ (c:)

instance ToRaw Raw where
  toRaw = id

instance ToRaw Resp where
  toRaw (Resp status hs c) = waiAddHeaders (map Hdr.cc hs) $ case c of
   Html hh hb -> r [utf8textHdr "html"] $ html2bl hh hb
   JSON json -> r [jsh] $ encode json
   Redirect url -> httpResponse 303 [Hdr.hdr Hdr.Location (TL.fromStrict url)] ""
   DiskFile path -> responseFile (waiCode status) [htmlUtf8 path False] (T.unpack path) Nothing
   InlineFile ct content -> r [ct] $ BL.fromStrict content
   Any status headers content -> httpResponse status headers content
   where
     jsh = Hdr.Header (Hdr.ContentType, "application/json; charset=UTF-8")
     r = httpResponse status

-- ** Shorthands

utf8ct :: TL.Text -> Resp -> Resp
utf8ct what r = r & headers .~ common
   where
   common :: [ Hdr.Header ]
   common = [ Hdr.Header (Hdr.ContentType, "text/"<>what<>"; charset=UTF-8") ]

addHead a r = r & content %~ f
   where f c = case c of Html h b -> Html (h>>a) b; _ -> c

modBody :: (E.Html -> E.Html) -> Resp -> Resp
modBody f r = withBody f r

prependBody a r = withBody (a>>) r
appendBody a r = withBody (>>a) r

withBody f r = r & content %~ f'
  where
    f' ra = case ra of
      Html h b -> Html h (f b)
      _ -> ra

-- * Conversion to Wai/Warp


-- * Helpers

-- ** Return response in monad

htmlBody   b = return . utf8ct "html" $ html' "" b :: Monad m => m Resp
textBody   b = return . utf8ct "html" $ html' "" b :: Monad m => m Resp
htmlPage h b = return . utf8ct "html" $ html' h  b :: Monad m => m Resp
redirect url = return . redirectResp $ url        :: Monad m => m Resp
file path = return . toResp $ DiskFile path :: Monad m => m Resp
inlineFile file = return . toResp $ file :: Monad m => m Resp
redirectResp url = utf8ct "html" $ Resp 200 [] $ Redirect url :: Resp

html' h b = Resp 200 [] (Html h b)

html2bl :: E.Html -> E.Html -> BL.ByteString
html2bl hh hb = renderHtml $ E.docType >> E.head hh >> E.body hb

waiAddHeaders hs r = case r of
   WaiI.ResponseBuilder st hdrs builder -> WaiI.ResponseBuilder st (hs <> hdrs) builder
   WaiI.ResponseFile st hdrs path mFilePart -> WaiI.ResponseFile st (hs <> hdrs) path mFilePart

htmlUtf8 fn bool = (hContentType, Mime.defaultMimeLookup fn <> (bool ? "; charset=UTF-8" $ ""))

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


-- * Receive

{-
TODO
   * implement POST-ing, postForm
   * convert to lens
   * Payload is now BS, but ToPayload renders to T (lazy text)
      - need to look what headers are (ASCII)
      - body can definitely be BS
module HTTP where

import Prelude2 hiding (unlines)
import Data.Word (Word8, Word16)

-- network, network-simple
import qualified Network.Simple.TCP as NS
import qualified Network.Socket.ByteString as N
import qualified Network.Socket as S

-- aeson
import qualified Data.Aeson as JSON
import Control.Lens hiding (un, (&))

import Text.Format

import           HTTP.Netw

-}


-- * Response parse

data ResponseR b where
   ResponseR :: BodyAs b => StatusLine -> [H.Header] -> b -> ResponseR b

type StatusLine = B.ByteString

parseResponse :: BodyAs b => B.ByteString -> ResponseR b
parseResponse bs = ResponseR statusLine headers body
   where (statusLine, rest) = B.breakSubstring crlf bs
         (hdrsBs, bodyBs)   = B.breakSubstring (crlf<>crlf) rest
         headers = parseHeaders hdrsBs
         body    = parseBody bodyBs
   -- ^ TODO: implement proper parsing


-- ** Parse response headers

parseHeaders :: B.ByteString -> [H.Header]
parseHeaders bs = pairs
   where rows = tokenise crlf bs :: [B.ByteString]
         br = B8.break (== ':')
         g = TLE.decodeUtf8 . BL.fromStrict
         f (b1,b2) = hdr (Hdr.Custom (g b1)) (g b2)
         pairs = map (f . br) rows

         tokenise x y = h : if B.null t
               then []
               else tokenise x (B.drop (B.length x) t)
            where (h,t) = B.breakSubstring x y


-- ** Parsing various types of response body

instance BodyAs B.ByteString where
   parseBody = id
instance BodyAs BL.ByteString where
   parseBody = BL.fromStrict
instance BodyAs TL.Text where
   parseBody = TLE.decodeUtf8 . BL.fromStrict
   -- ^ WARNING: this is unsafe, TODO

instance BodyAs (Maybe JSON.Object) where
   parseBody bs = JSON.decode (BL.fromStrict bs)


-- * Instances

deriving instance Show b => Show (ResponseR b)
-- deriving instance Show (Request b)
