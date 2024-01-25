module Cookie where

import Common.Prelude
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Time

import Render
import URL qualified

-- * Abstract cookie

data Field
  = Expires UTCTime
  | MaxAge Int
  | Domain TS.Text
  | Path URL.Path
  | Secure
  | HttpOnly
  | SameSiteStrict
  | SameSiteLax
  | SameSiteNone
  deriving (Show)

type Set a = [a]

data Cookie = Cookie
  { cookieName :: TS.Text
  , cookieValue :: TS.Text
  , cookieFields :: Set Field
  } deriving (Show)

makeFields ''Cookie

instance Render Cookie where
  type Conf Cookie = ()
  renderM c = return $ kv <> fs
    where
      kv :: TL.Text
      kv = TL.fromChunks [ c^.name, "=", c^.value ]
      fs :: TL.Text
      fs = TL.concat $ map ((";" <>) . render') (c^.fields)

instance Render Field where
  type Conf Field = ()
  renderM field = return $ case field of
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
    SameSiteNone -> "SameSite=None"

-- | Smart constructor with empty fields
cookie :: TS.Text -> TS.Text -> Cookie
cookie k v = Cookie k v []

secureCookie :: TS.Text -> TS.Text -> Cookie
secureCookie k v = cookie k v & fields .~ [Secure, HttpOnly]
