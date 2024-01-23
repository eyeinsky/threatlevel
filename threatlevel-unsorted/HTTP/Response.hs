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

-- import Data.FileEmbed
-- import Data.Text as TS
-- import Data.Text.Lazy as TL
-- import Data.Text.Encoding as TE
-- import Network.Mime as Mime
-- import qualified HTTP.Header as Hdr


-- * Opaque

type Raw = Wai.Response

class ToRaw a where
  toRaw :: a -> Raw

instance ToRaw Raw where
  toRaw = id

httpResponse :: Wai.Status -> [Wai.Header] -> BL.ByteString -> Wai.Response
httpResponse status headers body = responseBuilder status headers (BBB.fromLazyByteString body)

-- * Helpers

waiAddHeaders :: Wai.ResponseHeaders -> Wai.Response -> Wai.Response
waiAddHeaders hs r = case r of
   Wai.ResponseBuilder st hdrs builder -> Wai.ResponseBuilder st (hs <> hdrs) builder
   Wai.ResponseFile st hdrs path mFilePart -> Wai.ResponseFile st (hs <> hdrs) path mFilePart
   Wai.ResponseStream _ _ _ -> todo
   Wai.ResponseRaw _ _ -> todo

cacheForever :: Wai.Header
cacheForever = (Wai.hCacheControl, "max-age=31536000, public, immutable")

-- mkInlineFile path = let
--       ct = [| Hdr.header Hdr.ContentType (TL.fromStrict $ TE.decodeUtf8 $ Mime.defaultMimeLookup $ TS.pack path) |]
--    in [| InlineFile $ct $(embedFile path) |]
