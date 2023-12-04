{-# LANGUAGE TypeSynonymInstances #-}
module HTTP.Response where

import Common.Prelude

import qualified Data.ByteString.Lazy as BL
import qualified Blaze.ByteString.Builder as BBB

-- Wai/Warp conversions
import Network.Wai (responseBuilder)
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as Wai
import qualified Network.HTTP.Types as Wai

import qualified HTTP.Header as H
import qualified HTTP.Header as Hdr

-- * Opaque

type Raw = Wai.Response

class ToRaw a where
  toRaw :: a -> Raw

instance ToRaw Raw where
  toRaw = id

httpResponse :: Wai.Status -> [Hdr.Header] -> BL.ByteString -> Wai.Response
httpResponse status headers body
  = responseBuilder status headers' (BBB.fromLazyByteString body)
  where
    headers' = map Hdr.toWai headers

-- * Helpers

waiAddHeaders :: Wai.ResponseHeaders -> Wai.Response -> Wai.Response
waiAddHeaders hs r = case r of
   Wai.ResponseBuilder st hdrs builder -> Wai.ResponseBuilder st (hs <> hdrs) builder
   Wai.ResponseFile st hdrs path mFilePart -> Wai.ResponseFile st (hs <> hdrs) path mFilePart
   Wai.ResponseStream _ _ _ -> todo
   Wai.ResponseRaw _ _ -> todo

-- * Caching

cacheForever :: H.Header
cacheForever = H.header H.CacheControl "max-age=31536000, public, immutable"

-- * Receive

{-
TODO
   * implement POST-ing, postForm
   * convert to lens
   * Payload is now BS, but ToPayload renders to T (lazy text)
      - need to look what headers are (ASCII)
      - body can definitely be BS
module HTTP where

import X.Prelude hiding (unlines)
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

-- * Instances

-- deriving instance Show b => Show (Response b)
-- deriving instance Show (Request b)
