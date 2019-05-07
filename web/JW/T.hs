module JW.T where

import qualified Data.Text as TS
import qualified Data.Text.Strict.Lens as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as TL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Lens as A

import Prelude2


base64text :: BS.ByteString -> TL.Text
base64text bs = bs^.to B64.encode.lazy.TL.utf8

decode :: forall a. A.FromJSON a => TS.Text -> Either String (JWT a)
decode ts = do
  (a, b, c) <- case TS.splitOn "." ts of
    [a, b, c] -> pure (a, b, c)
    _ -> fail "Failed to split raw JWT to header, payload, signature"
  a' <- a^.l & maybe (fail "Failed to base64 decode header") pure
  b' <- b^.l & maybe (fail "Failed to base64 decode payload") pure
  b'' <- A.parseEither A.parseJSON b'
  pure $ JWT a' b'' c
  where l = re TS.utf8.to B64.decodeLenient.from strict.to A.decode

data JWT a = JWT
  { jWTHeader :: A.Value
  , jWTPayload :: a
  , jWTSignature :: TS.Text
  } deriving (Show)

makeFields ''JWT
