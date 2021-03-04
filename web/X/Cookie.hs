module X.Cookie where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import X.Prelude
import Data.Time
import URL (Path, render')
import HTTP.Common (ToPayload(..))
import qualified HTTP.Header as H
import Web.Response

-- * Abstract cookie

data Field
  = Expires UTCTime
  | MaxAge Int
  | Domain TS.Text
  | Path Path
  | Secure
  | HttpOnly
  | SameSiteStrict
  | SameSiteLax
  deriving (Show)

type Set a = [a]

data Cookie = Cookie
  { cookieName :: TS.Text
  , cookieValue :: TS.Text
  , cookieFields :: Set Field
  } deriving (Show)

makeFields ''Cookie

instance ToPayload Cookie where
  toPayload c = kv <> fs
    where
      kv :: TL.Text
      kv = TL.fromChunks [ c^.name, "=", c^.value ]
      fs :: TL.Text
      fs = TL.concat $ map ((";" <>) . toPayload) (c^.fields)

instance ToPayload Field where
  toPayload field = case field of
    Expires utc -> "Expires=" <> TL.pack (formatTime defaultTimeLocale fmt utc)
      where fmt = "%a, %d %b %Y %X GMT"
      {- ^ Expires=<date>, from https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie
         https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date#syntax
         Date: <day-name>, <day> <month> <year> <hour>:<minute>:<second> GMT -}
    MaxAge n -> "Max-Age=" <> TL.pack (show n)
    Domain ts -> "Domain=" <> TL.fromStrict ts
    Path p -> "Path=" <> render' p
    Secure -> "Secure"
    HttpOnly -> "HttpOnly"
    SameSiteStrict -> "SameSite=Strict"
    SameSiteLax -> "SameSite=Lax"

-- | Smart constructor with empty fields
cookie :: TS.Text -> TS.Text -> Cookie
cookie k v = Cookie k v []

secureCookie :: TS.Text -> TS.Text -> Cookie
secureCookie k v = cookie k v & fields .~ [Secure, HttpOnly]

-- * Interface to Response

setCookie :: Cookie -> Response -> Response
setCookie c = headers <>~ [H.header H.SetCookie (toPayload c)]

deleteCookie :: Cookie -> Response -> Response
deleteCookie cookie = headers <>~ [H.header H.SetCookie (toPayload cookie')]
  where
    cookie' = cookie & fields %~ (Expires inThePast :)
    inThePast = UTCTime (fromGregorian 1970 1 1) 1
    {- ^ "Thu, 01-Jan-1970 00:00:01 GMT"

       From https://tools.ietf.org/search/rfc6265#section-3.1 (search "Finally,
       to remove a cookie")

       To remove a cookie, in short:
       - set Expires to the past
       - use same path and domain -}
