module HTTP.Common where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString      as B

import X.Prelude

-- * Request payload type (TODO: to bs)

class ToPayload a where
   toPayload :: a -> T

instance ToPayload T where
   toPayload = id

type T = T.Text

unlines = T.unlines
pack    = T.pack
concat  = T.concat
null    = T.null

pair j (a,b) = a <> j <> b
un x xs = T.intercalate x xs

nl  = "\n"
nl2 = "\n\n"
crlf = "\r\n"

rp = TIO.putStr . toPayload


class BodyAs b where
   parseBody :: B.ByteString {-^ defined by NS.recv-} -> b
{- ^ We use BodyAs as a class to be able to extende it to
     arbitrary types. -}
