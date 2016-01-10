module HTTP_URL where

import Prelude2 hiding (null, un)
import qualified Prelude2 as P
import Text.Format

import HTTP_Common
import Data.Word (Word8, Word16)

data URL = URL {
     proto :: Proto
   {-user:pass@ :: Authority -}
   , host :: Host
   , port :: Port
   , path :: Path
   , params :: Params
   , fragment :: Fragment }

data Proto = Proto T
data Host
   = Domain T
   | IP4 Word8 Word8 Word8 Word8
newtype Port = Port { unPort :: Word16 }
data Path = Path [T]
data Params = Params [(T,T)]
data Fragment = Fragment T

{- Although called ToPayload, the method converts these for
   a payload to an HTTP Request and not for anything else
   -- but URI is more than that.
   -}

protoSep = "://"
portSep = ":"

instance ToPayload URL where
   toPayload (URL proto host port@ (Port pn) path params fragment) =
      HTTP_Common.concat [r proto, protoSep, r host, portSep, r port, r path, r params, r fragment]
      where r = toPayload

instance ToPayload Proto where
   toPayload (Proto a) = a

instance ToPayload Host where
   toPayload h = case h of
      Domain t -> t
      IP4 a b c d -> format "{}.{}.{}.{}" (a,b,c,d)

instance ToPayload Port where
   toPayload (Port w16) = w16 == 80 ? "" $ pack (show w16)

instance ToPayload Path where
   toPayload (Path p) = "/" <> un "/" p -- toPayload for Request => hast to start with /

instance ToPayload Params where
   toPayload (Params ps) = P.null ps ? "" $ expl
      where expl = "?" <> un "&" (map (pair "=") ps)

instance ToPayload Fragment where
   toPayload (Fragment a) = null a ? "" $ expl
      where expl = "#" <> a
