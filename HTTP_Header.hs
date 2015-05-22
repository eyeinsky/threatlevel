module HTTP_Header where

import Prelude2 hiding (unlines, find)
import Text.Format

import HTTP_Common

-- for conversion to Network.HTTP.Types.Header
import qualified Network.HTTP.Types.Header as H
import qualified Data.CaseInsensitive      as CI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import qualified Data.Text       as T
import qualified Data.Text.Lazy  as TL
import qualified Data.Text.Encoding as TE

newtype Header = Header (HeaderName, T)
hdr a b = Header (a, b)

instance ToPayload Header where
   toPayload (Header (k, v)) = find k <> ": " <> v
instance ToPayload [Header] where
   toPayload = unlines . map toPayload

find k = fromMaybe (error "header missing from map") $ lookup k headerMap

data HeaderName where --   Stolen from "HTTP": Network.HTTP.Headers
    -- Generic Headers
   CacheControl :: HeaderName
   Connection :: HeaderName
   Date :: HeaderName
   Pragma :: HeaderName
   TransferEncoding :: HeaderName
   Upgrade :: HeaderName
   Via :: HeaderName
    -- Request Headers
   Accept :: HeaderName
   AcceptCharset :: HeaderName
   AcceptEncoding :: HeaderName
   AcceptLanguage :: HeaderName
   Authorization :: HeaderName
   Cookie :: HeaderName
   Expect :: HeaderName
   From :: HeaderName
   Host :: HeaderName
   IfModifiedSince :: HeaderName
   IfMatch :: HeaderName
   IfNoneMatch :: HeaderName
   IfRange :: HeaderName
   IfUnmodifiedSince :: HeaderName
   MaxForwards :: HeaderName
   ProxyAuthorization :: HeaderName
   Range :: HeaderName
   Referer :: HeaderName
   UserAgent :: HeaderName
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
   , p "Accept"               Accept
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
   ]
  where p a b = (b,a)

-- 
accHtml = "text/html"

deriving instance Show HeaderName
deriving instance Eq HeaderName
deriving instance Show Header
deriving instance Eq Header


-- Conversion to http-types headers (for Wai/Warp)

cc (Header (hn, t)) = (n,v) :: H.Header
   where n = CI.mk $ f $ find hn
         v = f t
         f = TE.encodeUtf8 . TL.toStrict


-- * Cookie

data Cookie = Cookie'
   { name :: T
   , value :: T
   , domain :: T
   , cPath :: [T]
   , expires :: T
   }

instance ToPayload Cookie where
   toPayload (Cookie' k v d p e) = un ";" $ filter (not . TL.null)
         [ x, d', p', e'
         ]
      where
         x  = format "{}={}" (k, v)
         d' = format "Domain={}" [d]
         p' = format "Path={}" ["/" <> un "/" p]
         e' = format "Expires={}" [e]

cookie' a b c d e = Header (SetCookie, toPayload $ Cookie' a b c d e)
mkC k v = cookie' k v "typorg.dev" [] ""
delC k  = cookie' k "deleted" "typorg.dev" [] "Thu, 01-Jan-1970 00:00:01 GMT"
