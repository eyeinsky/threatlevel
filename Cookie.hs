module Cookie where

import Prelude2 hiding (un)
import Text.Format
import qualified Data.Text.Lazy as TL
import HTTP.Common

data Cookie = Cookie
   { name    :: TL.Text
   , value   :: TL.Text
   , domain  :: Maybe TL.Text
   , cPath   :: Maybe [TL.Text]
   , expires :: Maybe TL.Text
   }


cookieString :: Cookie -> TL.Text
cookieString (Cookie k v d p e) = un ";" $ x : catMaybes
      [d', p', e' ]
   where
      x  = format "{}={}" (k, v)
      d' = f1 "Domain={}" <$> d
      p' = (\p -> f1 "Path={}" ("/" <> un "/" p)) <$> p
      e' = f1 "Expires={}" <$> e
      f1 s p = format s [p]

-- TODO: omit empty strings
-- cookieShort (Cookie k v d p e) = un ";" $ filter (not . TL.null)
