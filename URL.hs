module URL where

import Prelude2 hiding (null, un)
import qualified Prelude2 as P
import Text.Format

import HTTP.Common
import Data.Word (Word8, Word16)

data URL = URL {
     proto :: Proto
   , authority :: Authority
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

data Authority = Authority {
     authentication :: Maybe (T, T)
   , host :: Host
   , port :: Port
   }

data BaseURL = BaseURL URL.Proto URL.Host URL.Port
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


{- Although called ToPayload, the method converts these for
   a payload to an HTTP Request and not for anything else
   -- but URI is more than that.
   -}

protoSep = "://"
portSep = ":"

instance ToPayload URL where
   toPayload (URL proto authority path params fragment) =
      HTTP.Common.concat [r proto, protoSep, r authority, r path, r params, r fragment]
      where r = toPayload

instance ToPayload Authority where
   toPayload (Authority authentication host port@ (Port pn)) =
      HTTP.Common.concat [maybe "" mkAuth authentication, r host, portSep, r port]
      where r = toPayload
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
   toPayload (Path p) = "/" <> un "/" p -- toPayload for Request => hast to start with /

instance ToPayload Params where
   toPayload (Params ps) = P.null ps ? "" $ expl
      where expl = "?" <> un "&" (map (pair "=") ps)

instance ToPayload Fragment where
   toPayload (Fragment a) = null a ? "" $ expl
      where expl = "#" <> a

-- ** Instances

deriving instance Eq URL
deriving instance Eq Authority
deriving instance Eq Proto
deriving instance Eq Host
deriving instance Eq Port
deriving instance Eq Path
deriving instance Eq Params
deriving instance Eq Fragment

deriving instance Ord Port

deriving instance Show URL
deriving instance Show Authority
deriving instance Show Proto
deriving instance Show Host
deriving instance Show Port
deriving instance Show Path
deriving instance Show Params
deriving instance Show Fragment
