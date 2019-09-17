module URL.ToPayload
  ( module URL.ToPayload
  , module URL
  ) where

import X.Prelude hiding (null, un)
import qualified X.Prelude as P
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text as T
import Data.Text.Format

import URL hiding (T)
import HTTP.Common hiding (un)
import Network.HTTP.Types
import Data.Hashable (Hashable)

deriving instance Generic Port
deriving instance Hashable Port

instance ToPayload BaseURL where
   toPayload (BaseURL proto@ (Proto proto') host port@ (Port port')) =
         toPayload proto
      <> "://"
      <> toPayload host
      <> portPayload
     where
       portPayload
         | proto' == "http" && port' == 80 = ""
         | proto' == "https" && port' == 443 = ""
         | otherwise = ":" <> toPayload port

withoutSchema (BaseURL proto@ (Proto proto') host port@ (Port port')) =
     toPayload host
  <> portPayload
  where
    portPayload
      | proto' == "http" && port' == 80 = ""
      | proto' == "https" && port' == 443 = ""
      | otherwise = ":" <> toPayload port

instance ToPayload URL where
   toPayload (URL proto authority path params fragment) =
      HTTP.Common.concat
        [ r proto, "://"
        , auth, authority^.host.to r, portPayload
        , r path, r params, r fragment]
      where
        r = toPayload
        auth = maybe "" mkAuth $ authority^.authentication
        portPayload
          | pr == "http" && po == 80 = ""
          | pr == "https" && po == 443 = ""
          | otherwise = ":" <> toPayload (authority^.port)
          where
            pr = proto^.un
            po = authority^.port.un

instance ToPayload Authority where
   toPayload a = TL.concat
     [ maybe "" mkAuth $ a^.authentication
     , r $ a^.host
     , ":"
     , r $ a^.port
     ]
     where
       r = toPayload

mkAuth (u, p) = TL.fromStrict $ u <> ":" <> p <> "@"

instance ToPayload Proto where
   toPayload (Proto a) = TL.fromStrict a

instance ToPayload Host where
   toPayload h = case h of
      Domain t -> TL.fromStrict t
      IP4 a b c d -> format "{}.{}.{}.{}" (a,b,c,d)

instance ToPayload Port where
   toPayload (Port w16) = pack (show w16)

instance ToPayload Path where
   toPayload (Path p) = "/" <> TL.intercalate "/" (map TL.fromStrict p)
   -- toPayload for Request => hast to start with /

instance ToPayload Params where
   toPayload (Params ps) = P.null ps ? "" $ expl
      where
        expl = "?" <> TL.intercalate "&" (map f ps)
        f (a, b) = maybe a' (\b' -> a' <> "=" <> TL.fromStrict (urlEncode' b')) b
          where
            a' = TL.fromStrict a

instance ToPayload Fragment where
   toPayload (Fragment a) = TL.fromStrict $ T.null a ? "" $ expl
      where expl = "#" <> a

urlEncode' :: TS.Text -> TS.Text
urlEncode' = TS.encodeUtf8 ^ urlEncode True ^ TS.decodeUtf8
-- ^ True: encodes ","
