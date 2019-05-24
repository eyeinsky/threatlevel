module CSP where

import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as TL
import qualified Data.Text as TS
import qualified Data.Text.Lens as TS
import qualified Crypto.Hash as C

import qualified Data.ByteArray.Encoding as BA

import X.Prelude

-- | A base64 encoding of a binary hash
sha256 :: TL.Text -> TL.Text
sha256 t = BA.convertToBase BA.Base64 d ^. from strict . TL.utf8
  where
    d = t^.re TL.utf8.from lazy.to C.hash :: C.Digest C.SHA256
{- The sha256 of "alert('Hello, world.');" is
   "sha256-qznLcsROx4GACP2dm0UCKCzCG+HiZ1guq6ZZDob/Tng=".

   On the command line it's:
   $ echo -n "alert('Hello, world.');" | openssl sha256 -binary | openssl enc -base64
-}
