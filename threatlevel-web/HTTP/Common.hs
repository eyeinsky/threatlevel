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

unlines :: [T.Text] -> T.Text
unlines = T.unlines

pack :: String -> T.Text
pack = T.pack

concat :: [T.Text] -> T.Text
concat = T.concat

null :: T.Text -> Bool
null = T.null

pair :: Semigroup a => a -> (a, a) -> a
pair j (a,b) = a <> j <> b

un :: T.Text -> [T.Text] -> T.Text
un x xs = T.intercalate x xs

nl :: IsString p => p
nl  = "\n"

nl2 :: IsString p => p
nl2 = "\n\n"

crlf :: IsString p => p
crlf = "\r\n"

rp :: ToPayload a => a -> IO ()
rp = TIO.putStr . toPayload


class BodyAs b where
   parseBody :: B.ByteString {-^ defined by NS.recv-} -> b
{- ^ We use BodyAs as a class to be able to extende it to
     arbitrary types. -}
