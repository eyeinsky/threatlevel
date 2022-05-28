{-# OPTIONS_GHC -Wno-missing-methods #-}
module URL.Core where

import Prelude
import Data.String
import Data.Word (Word8, Word16)
import qualified Data.Text as TS
import Data.Text.Lens
import Control.Lens
import GHC.Generics (Generic)

-- * Absolute

newtype Proto = Proto TS.Text

instance IsString Proto where
  fromString = view (packed.to Proto)

data Host
   = Domain TS.Text
   | IP4 Word8 Word8 Word8 Word8

makePrisms ''Host

instance IsString Host where
  fromString = view (packed.to Domain)

instance Read Host where
  readsPrec _ str = [(fromString str, "")]

newtype Port = Port Word16

instance Num Port where
  fromInteger a = Port (fromInteger a)

instance Read Port where
  readsPrec _ str = [(Port (read str), "")]

deriving instance Generic Port

type Segment = TS.Text
newtype Path = Path { pathSegments :: [Segment] }
makeFields ''Path

instance Semigroup Path where
  Path a <> Path b = Path (a <> b)
instance Monoid Path where
  mempty = Path mempty

newtype Param = Param (TS.Text, Maybe TS.Text)
newtype Params = Params [Param]

instance Semigroup Params where
  Params a <> Params b = Params (a <> b)
instance Monoid Params where
  mempty = Params mempty

newtype Fragment = Fragment TS.Text
  deriving newtype (Semigroup, Monoid)

declareFields [d|
  data Authority = Authority
    { authorityAuthentication :: Maybe (TS.Text, TS.Text)
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

instance HasSegments URL [TS.Text] where
  segments = path . segments

instance HasSegments [Segment] [TS.Text] where
  segments = id

baseUrl :: Lens' URL BaseURL
baseUrl f url = fmap to (f from)
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
deriving instance Eq Param
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
deriving instance Show Param
deriving instance Show Params
deriving instance Show Fragment

-- * Convenience

param :: TS.Text -> TS.Text -> URL -> URL
param k v = params . coerced <>~ [Param (k, Just v)]

paramKey :: TS.Text -> URL -> URL
paramKey k = params . coerced <>~ [Param (k, Nothing)]
