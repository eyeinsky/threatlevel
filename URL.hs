module URL where

import Pr hiding (null, un)
import qualified Prelude2 as P
import qualified Data.Text.Lazy as TL
import Data.Text.Format

import HTTP.Common hiding (un)
import Data.Word (Word8, Word16)
import Data.Hashable (Hashable)


protoSep = "://"
portSep = ":"

declareFields [d|
  data Proto = Proto
    { protoUn :: T }
  |]

data Host
   = Domain T
   | IP4 Word8 Word8 Word8 Word8

makePrisms ''Host

instance IsString Host where
  fromString = view (packed.to Domain)

instance Read Host where
  readsPrec _ str = [(fromString str, "")]

declareFields [d|
  newtype Port = Port { portUn :: Word16 }
  |]

deriving instance Hashable Port

instance Num Port where
  fromInteger a = Port (fromInteger a)

instance Read Port where
  readsPrec _ str = [(Port (read str), "")]

declareFields [d|
  data Path = Path { pathSegments :: [T] }
  |]

declareFields [d|
  data Params = Params { paramsUn :: [(T,T)] }
  |]
data Fragment = Fragment T

declareFields [d|
  data Authority = Authority
    { authorityAuthentication :: Maybe (T, T)
    , authorityHost :: Host
    , authorityPort :: Port
    }
  |]

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

declareFields [d|
  data URL = URL
    { uRLProto :: Proto
    , uRLAuthority :: Authority
    , uRLPath :: Path
    , uRLParams :: Params
    , uRLFragment :: Fragment }
  |]

localhost = URL (Proto "http") auth (Path []) (Params []) (Fragment "")
  where
    auth = Authority Nothing (Domain "localhost") $ Port 80

-- * Helpers

base :: Lens' URL BaseURL
base f url = fmap to (f from)
  where
    auth = url^.authority
    from = BaseURL (url^.proto) (auth^.host) (auth^.port)
    to (BaseURL pr ho po) = URL pr (Authority Nothing ho po) path params fragment
      where
        path = Path []
        params = Params []
        fragment = Fragment ""



{- Although called ToPayload, the method converts these for
   a payload to an HTTP Request and not for anything else
   -- but URI is more than that.
   -}

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
