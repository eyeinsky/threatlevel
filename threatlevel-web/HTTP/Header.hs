{-# OPTIONS_GHC -Wno-orphans #-}
module HTTP.Header
  ( toWai
  , header
  , HeaderName(..), Header

  -- * Helpers
  , contentType, javascript, json, utf8text
  ) where


-- for conversion to Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Header as H
import qualified Data.CaseInsensitive      as CI
import qualified Data.Text.Lazy  as TL
import qualified Data.Text.Encoding as TE

import X.Prelude hiding (unlines, find)

import HTTP.Common


newtype Header = Header (HeaderName, T)

-- | Helper to create a Header
header :: HeaderName -> T -> Header
header h v = Header (h, v)

instance ToPayload Header where
   toPayload (Header (k, v)) = find k headerMap <> ": " <> v
instance ToPayload [Header] where
   toPayload = unlines . map toPayload

find :: Eq a1 => a1 -> [(a1, a2)] -> a2
find k m = fromMaybe (error "header missing from map") $ lookup k m

data HeaderName where --   Stolen from "HTTP": Network.HTTP.Headers

    -- Generic Headers
   CacheControl :: HeaderName
   Connection :: HeaderName
   Date :: HeaderName
   Pragma :: HeaderName
   TransferEncoding :: HeaderName
   Upgrade :: HeaderName
   Via :: HeaderName

    -- Response Headers
   Age :: HeaderName
   Location :: HeaderName
   ProxyAuthenticate :: HeaderName
   Public :: HeaderName
   RetryAfter :: HeaderName
   Server :: HeaderName
   SetCookie :: HeaderName
   TE :: HeaderName
   Trailer :: HeaderName
   Vary :: HeaderName
   Warning :: HeaderName
   WWWAuthenticate :: HeaderName
   StrictTransportSecurity :: HeaderName
   ContentDisposition :: HeaderName
   -- ** CSP
   ContentSecurityPolicy :: HeaderName
   ContentSecurityPolicyReportOnly :: HeaderName

    -- Entity Headers :: HeaderName
   Allow :: HeaderName
   ContentBase :: HeaderName
   ContentEncoding :: HeaderName
   ContentLanguage :: HeaderName
   ContentLength :: HeaderName
   ContentLocation :: HeaderName
   ContentMD5 :: HeaderName
   ContentRange :: HeaderName
   ContentType :: HeaderName
   ETag :: HeaderName
   Expires :: HeaderName
   LastModified :: HeaderName

    -- | MIME entity headers (for sub-parts)
   ContentTransferEncoding :: HeaderName

    -- | Allows for unrecognised or experimental headers.
   Custom :: T -> HeaderName -- not in header map below.


headerMap :: [ (HeaderName, T) ]
headerMap =
   [ p "Cache-Control"        CacheControl
   , p "Connection"           Connection
   , p "Date"                 Date
   , p "Pragma"               Pragma
   , p "Transfer-Encoding"    TransferEncoding
   , p "Upgrade"              Upgrade
   , p "Via"                  Via

   , p "Age"                  Age
   , p "Location"             Location
   , p "Proxy-Authenticate"   ProxyAuthenticate
   , p "Public"               Public
   , p "Retry-After"          RetryAfter
   , p "Server"               Server
   , p "Set-Cookie"           SetCookie
   , p "TE"                   TE
   , p "Trailer"              Trailer
   , p "Vary"                 Vary
   , p "Warning"              Warning
   , p "WWW-Authenticate"     WWWAuthenticate
   , p "Strict-Transport-Security" StrictTransportSecurity
   , p "Content-Disposition"  ContentDisposition
   , p "Allow"                Allow
   , p "Content-Base"         ContentBase
   , p "Content-Encoding"     ContentEncoding
   , p "Content-Language"     ContentLanguage
   , p "Content-Length"       ContentLength
   , p "Content-Location"     ContentLocation
   , p "Content-MD5"          ContentMD5
   , p "Content-Range"        ContentRange
   , p "Content-Type"         ContentType
   , p "ETag"                 ETag
   , p "Expires"              Expires
   , p "Last-Modified"        LastModified
   , p "Content-Transfer-Encoding" ContentTransferEncoding
   , p "Content-Security-Policy" ContentSecurityPolicy
   , p "Content-Security-Policy-Report-Only" ContentSecurityPolicyReportOnly
   ]

p :: b -> a -> (a, b)
p a b = (b,a)

deriving instance Show HeaderName
deriving instance Eq HeaderName
deriving instance Show Header
deriving instance Eq Header


-- Conversion to http-types headers (for Wai/Warp)

toWai :: Header -> H.Header
toWai (Header (hn, t)) = (n,v)
  where
    n = CI.mk $ f $ find hn headerMap
    v = f t
    f = TE.encodeUtf8 . TL.toStrict

-- * Request header

newtype RequestHeader = RequestHeader
   { _unRequestHeader :: (RequestHeaderName, T) }

data RequestHeaderName where
   Accept :: RequestHeaderName
   AcceptCharset :: RequestHeaderName
   AcceptEncoding :: RequestHeaderName
   AcceptLanguage :: RequestHeaderName
   Authorization :: RequestHeaderName
   Cookie :: RequestHeaderName
   Expect :: RequestHeaderName
   From :: RequestHeaderName
   Host :: RequestHeaderName
   IfModifiedSince :: RequestHeaderName
   IfMatch :: RequestHeaderName
   IfNoneMatch :: RequestHeaderName
   IfRange :: RequestHeaderName
   IfUnmodifiedSince :: RequestHeaderName
   MaxForwards :: RequestHeaderName
   ProxyAuthorization :: RequestHeaderName
   Range :: RequestHeaderName
   Referer :: RequestHeaderName
   UserAgent :: RequestHeaderName
   deriving (Eq, Ord)

instance ToPayload RequestHeader where
   toPayload (RequestHeader (name, _)) = find name map
      where
         map = [
              p "Accept"               Accept
            , p "Accept-Charset"       AcceptCharset
            , p "Accept-Encoding"      AcceptEncoding
            , p "Accept-Language"      AcceptLanguage
            , p "Authorization"        Authorization
            , p "Cookie"               Cookie
            , p "Expect"               Expect
            , p "From"                 From
            , p "Host"                 Host
            , p "If-Modified-Since"    IfModifiedSince
            , p "If-Match"             IfMatch
            , p "If-None-Match"        IfNoneMatch
            , p "If-Range"             IfRange
            , p "If-Unmodified-Since"  IfUnmodifiedSince
            , p "Max-Forwards"         MaxForwards
            , p "Proxy-Authorization"  ProxyAuthorization
            , p "Range"                Range
            , p "Referer"              Referer
            , p "User-Agent"           UserAgent
            ]

-- * Shorthands

contentType :: T -> Header
contentType val = Header (ContentType, val)

javascript :: Header
javascript = contentType "application/javascript; charset=utf-8"

json :: Header
json = contentType "application/json; charset=UTF-8"

utf8text :: T -> Header
utf8text what = contentType ("text/"<>what<>"; charset=UTF-8")
