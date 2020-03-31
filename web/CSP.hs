module CSP where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as TL
import qualified Data.Text as TS
import qualified Crypto.Hash as C

import qualified Data.ByteArray.Encoding as BA

import Render
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

-- * Syntax

data Value
  = Self
  | UnsafeInline
  | UnsafeEval
  | Nonce TS.Text
  | Sha256 TS.Text
  -- | Urls
  | AnyUrl
  | Url TS.Text
  | Domain TS.Text
  | Protocol TS.Text
  | Data

declareFields [d|
  data CSP = CSP
    { cSPDefaultSrc :: [Value]
    , cSPScriptSrc :: [Value]
    , cSPStyleSrc :: [Value]
    , cSPImgSrc :: [Value]
    , cSPFontSrc :: [Value]
    , cSPObjectSrc :: [Value]
    , cSPMediaSrc :: [Value]
    , cSPConnectSrc :: [Value]
    , cSPreportUri :: TS.Text
    }
  |]

instance Default CSP where
  def = CSP mempty mempty mempty mempty mempty mempty mempty mempty mempty

instance Render Value where
  type Conf Value = ()
  renderM v = pure $ case v of
    Self -> q "self"
    UnsafeInline -> q "unsafe-inline"
    UnsafeEval -> q "unsafe-eval"
    Nonce t -> q $ "nonce-" <> TL.fromStrict t
    Sha256 t -> q $ "sha256-" <> TL.fromStrict t
    -- | Urls
    AnyUrl -> "*"
    Url t -> TL.fromStrict t
    Domain t -> TL.fromStrict t
    Protocol t -> TL.fromStrict t <> ":"
    Data -> "data:"
    where
      q v = "'" <> v <> "'"

instance Render CSP where
  renderM csp = filter (not . null) li
    & map TL.unwords
    & TL.intercalate ";"
    & pure
    where
      f lens _ = map render' (csp ^. lens :: [Value]) :: [TL.Text]
      li =
        [ f defaultSrc "default-src"
        , f scriptSrc "script-src"
        , f styleSrc "style-src"
        , f imgSrc "img-src"
        , f fontSrc "font-src"
        , f objectSrc "object-src"
        , f mediaSrc "media-src"
        , f connectSrc "connect-src"
        ]

-- Level 2

-- childSrc = todo
-- formAction = todo
-- formAncestors = todo
-- pluginTypes = todo

{-
url v = tell [C.Url v]
      urls :: [C.Value]
      urls = execWriter $ do
        url "https://cdnjs.cloudflare.com"
        url "https://cdn.datatables.net"
        url "https://unpkg.com"
        url "https://maps.googleapis.com"
        url "https://cdn.datatables.net"
        url "https://nytida.se"
        url "https://www.heimta.no"
        url "https://www.vardaga.se"
        url "https://www.altiden.dk"
        url "https://*.fbcdn.net"
        url "https://www.googletagmanager.com"
        url "https://www.google-analytics.com"
      csp' = def
        & defaultSrc <>~ [Self, UnsafeInline, UnsafeEval, C.Data]
        & defaultSrc <>~ urls
        :: CSP
-}
