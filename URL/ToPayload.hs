module URL.ToPayload
  ( module URL.ToPayload
  , module URL
  ) where

import Pr hiding (null, un)
import qualified Prelude2 as P
import qualified Data.Text.Lazy as TL
import Data.Text.Format

import URL hiding (T)
import HTTP.Common hiding (un)
import Data.Hashable (Hashable)

deriving instance Hashable Port

instance ToPayload BaseURL where
   toPayload (BaseURL proto@ (Proto proto') host port @(Port port')) =
         toPayload proto
      <> protoSep
      <> toPayload host
      <> portPayload
     where
       portPayload
         | proto' == "http" && port' == 80 = ""
         | proto' == "https" && port' == 443 = ""
         | otherwise = portSep <> toPayload port

withoutSchema (BaseURL proto@ (Proto proto') host port @(Port port')) =
     toPayload host
  <> portPayload
  where
    portPayload
      | proto' == "http" && port' == 80 = ""
      | proto' == "https" && port' == 443 = ""
      | otherwise = portSep <> toPayload port

instance ToPayload URL where
   toPayload (URL proto authority path params fragment) =
      HTTP.Common.concat
        [ r proto, protoSep
        , auth, authority^.host.to r, portPayload
        , r path, r params, r fragment]
      where
        r = toPayload
        auth = maybe "" mkAuth $ authority^.authentication
        portPayload
          | pr == "http" && po == 80 = ""
          | pr == "https" && po == 443 = ""
          | otherwise = portSep <> toPayload (authority^.port)
          where
            pr = proto^.un
            po = authority^.port.un

instance ToPayload Authority where
   toPayload a = HTTP.Common.concat
     [ maybe "" mkAuth $ a^.authentication
     , r $ a^.host
     , portSep
     , r $ a^.port
     ]
     where
       r = toPayload

mkAuth (u, p) = u <> ":" <> p <> "@"

instance ToPayload Proto where
   toPayload (Proto a) = a

instance ToPayload Host where
   toPayload h = case h of
      Domain t -> t
      IP4 a b c d -> format "{}.{}.{}.{}" (a,b,c,d)

instance ToPayload Port where
   toPayload (Port w16) = pack (show w16)

instance ToPayload Path where
   toPayload (Path p) = "/" <> TL.intercalate "/" p -- toPayload for Request => hast to start with /

instance ToPayload Params where
   toPayload (Params ps) = P.null ps ? "" $ expl
      where expl = "?" <> TL.intercalate "&" (map (pair "=") ps)

instance ToPayload Fragment where
   toPayload (Fragment a) = null a ? "" $ expl
      where expl = "#" <> a
