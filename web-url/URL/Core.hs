{-# OPTIONS_GHC -Wno-missing-methods #-}
module URL.Core where

import Prelude
import Data.String
import Data.Word (Word8, Word16)
import qualified Data.Text as T
import Data.Text.Lens
import Control.Lens
import GHC.Generics (Generic)

type T = T.Text

-- * Absolute

declareFields [d|
  data Proto = Proto
    { protoUn :: T }
  |]

instance IsString Proto where
  fromString = view (packed.to Proto)

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

instance Num Port where
  fromInteger a = Port (fromInteger a)

instance Read Port where
  readsPrec _ str = [(Port (read str), "")]

deriving instance Generic Port

type Segment = T
declareFields [d|
  data Path = Path { pathSegments :: [Segment] }
  |]
instance Semigroup Path where
  Path a <> Path b = Path (a <> b)
instance Monoid Path where
  mempty = Path mempty

declareFields [d|
  data Params = Params { paramsUn :: [(T, Maybe T)] }
  |]
instance Semigroup Params where
  Params a <> Params b = Params (a <> b)
instance Monoid Params where
  mempty = Params mempty

newtype Fragment = Fragment T
  deriving newtype (Semigroup, Monoid)

declareFields [d|
  data Authority = Authority
    { authorityAuthentication :: Maybe (T, T)
    , authorityHost :: Host
    , authorityPort :: Port
    }
  |]

declareFields [d|
  data URL = URL
    { uRLProto :: Proto
    , uRLAuthority :: Authority
    , uRLPath :: Path
    , uRLParams :: Params
    , uRLFragment :: Fragment }
  |]

-- * Relative

declareFields [d|
  data PathParamsFragment = PathParamsFragment
    { relativePath :: Path
    , relativeParams :: Params
    , relativeFragment :: Fragment
    }
  |]

-- * All

data WebURL
  = Full URL
  | AbsolutePath PathParamsFragment
  | RelativePath PathParamsFragment
  | FragmentOnly Fragment

data BaseURL = BaseURL Proto Host Port

-- * Additional lens instances

instance HasHost URL Host where
  host = authority.host

instance HasPort URL Port where
  port = authority.port

instance HasSegments URL [T] where
  segments = path . segments

instance HasSegments [Segment] [T] where
  segments = id

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

-- * Instances

deriving instance Eq WebURL
deriving instance Eq PathParamsFragment
deriving instance Eq URL
deriving instance Eq Authority
deriving instance Eq Proto
deriving instance Eq Host
deriving instance Eq Port
deriving instance Eq Path
deriving instance Eq Params
deriving instance Eq Fragment

deriving instance Ord Port

deriving instance Show WebURL
deriving instance Show PathParamsFragment
deriving instance Show URL
deriving instance Show Authority
deriving instance Show Proto
deriving instance Show Host
deriving instance Show Port
deriving instance Show Path
deriving instance Show Params
deriving instance Show Fragment
