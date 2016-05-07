module Web.Blaze where

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as E

jsTag = E.script E.! A.type_ "text/javascript"
cssTag = E.style E.! A.type_ "text/css"
favicon adr = E.link
   E.! A.rel "shortcut icon"
   E.! A.type_ "image/x-icon"
   E.! A.href (E.toValue adr)
