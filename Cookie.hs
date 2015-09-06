
module Cookie where

import Prelude2 hiding (un)
import Text.Format
import qualified Data.Text.Lazy as TL
import HTTP_Common

data Cookie = Cookie
   { name    :: TL.Text
   , value   :: TL.Text
   , domain  :: TL.Text
   , cPath   :: [TL.Text]
   , expires :: TL.Text
   }


cookieString :: Cookie -> TL.Text
cookieString (Cookie k v d p e) = un ";" $ filter (not . TL.null)
      [ x, d', p', e' ]
   where
      x  = format "{}={}" (k, v)
      d' = format "Domain={}" [d]
      p' = format "Path={}" ["/" <> un "/" p]
      e' = format "Expires={}" [e]

-- TODO: omit empty strings
-- cookieShort (Cookie k v d p e) = un ";" $ filter (not . TL.null)
