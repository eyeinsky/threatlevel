module X.Cookie where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import X.Prelude
import Data.Time
import URL (Path, render')
import HTTP.Common (ToPayload(..))
import qualified HTTP.Header as H
import Web.Response

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

secureCookie :: TS.Text -> TS.Text -> UTCTime -> Cookie
secureCookie k v expires = cookie k v & fields .~ Expires expires : secure
  where secure = [Secure, HttpOnly]

setCookie :: Cookie -> Response -> Response
setCookie c = headers <>~ [H.header H.SetCookie (toPayload c)]
