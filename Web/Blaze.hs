module Web.Blaze where

import Prelude
import qualified Data.Text.Lazy as TL

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as E

import qualified Web_CSS as CSS

jsTag = E.script E.! A.type_ "text/javascript"
jsUrl url = jsTag E.! A.src url $ ""
cssTag = E.style E.! A.type_ "text/css"
cssUrl path = E.link E.! A.type_ "text/css" E.! A.rel "stylesheet" E.! A.href (E.toValue path)
favicon adr = E.link
   E.! A.rel "shortcut icon"
   E.! A.type_ "image/x-icon"
   E.! A.href (E.toValue adr)

cls_ strs = A.class_ $ E.toValue $ TL.unwords $ map CSS.unClass strs
id_ (CSS.Id t) = A.id $ E.toValue t
