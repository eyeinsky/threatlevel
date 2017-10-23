module HTTP.Response where

import Pr hiding (Any)

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
import URL.ToPayload

-- * Opaque

type Raw = Wai.Response

class ToRaw a where
  toRaw :: a -> Raw

instance ToRaw Raw where
  toRaw = id

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

-- * HTTP response

declareFields [d|
  data Response = Response
    { responseCode :: Int
    , responseHeaders :: [H.Header]
    , responseBody :: BL.ByteString
    }
  |]

class ToResponse a where
  toResponse :: a -> Response

instance ToRaw Response where
  toRaw (Response status headers content) = httpResponse status headers content

-- * Helpers

utf8textHdr what = Hdr.Header (Hdr.ContentType, "text/"<>what<>"; charset=UTF-8")

waiAddHeaders hs r = case r of
   WaiI.ResponseBuilder st hdrs builder -> WaiI.ResponseBuilder st (hs <> hdrs) builder
   WaiI.ResponseFile st hdrs path mFilePart -> WaiI.ResponseFile st (hs <> hdrs) path mFilePart

htmlUtf8 fn bool = (hContentType, Mime.defaultMimeLookup fn <> (bool ? "; charset=UTF-8" $ ""))

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

{-
parseResponse :: BodyAs b => B.ByteString -> Response b
parseResponse bs = Response statusLine headers body
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
-}

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

-- deriving instance Show b => Show (Response b)
-- deriving instance Show (Request b)
