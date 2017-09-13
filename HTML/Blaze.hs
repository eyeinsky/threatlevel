module HTML.Blaze where

import Prelude as Pr
import Data.String (fromString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html5            as E

import qualified CSS as CSS
import DOM.Event (Event, toOn)
import JS
import JS.Blaze
import DOM.Core

import HTML hiding (jsTag, cssTag, favicon)

jsTag = E.script E.! A.type_ "text/javascript"
jsUrl url = jsTag E.! A.src url $ ""
cssTag = E.style E.! A.type_ "text/css"
cssUrl path = E.link E.! A.type_ "text/css" E.! A.rel "stylesheet" E.! A.href (E.toValue path)
favicon adr = E.link
   E.! A.rel "shortcut icon"
   E.! A.type_ "image/x-icon"
   E.! A.href (E.toValue adr)

cls_ strs = A.class_ $ E.toValue $ TL.unwords $ Pr.map (static . unClass) strs
id_ (Id t) = A.id $ E.toValue $ static t

on :: Event a => a -> Expr a1 -> E.Attribute
on event js = customAttribute (fromString $ TL.unpack $ toOn event) (toValue $ call1 js (ex "event"))

instance E.ToMarkup HTML.Html where
  toMarkup = preEscapedToMarkup
  preEscapedToMarkup = E.preEscapedToMarkup . render
instance E.ToMarkup HTML where
  toMarkup = preEscapedToMarkup
  preEscapedToMarkup = E.preEscapedToMarkup . render
instance E.ToMarkup [HTML] where
  toMarkup = preEscapedToMarkup
  preEscapedToMarkup = E.preEscapedToMarkup . render

-- * Exclamatable instances

instance Exclamatable (E.Html) E.Attribute where
  (!) e a = e E.! a
instance Exclamatable (E.Html -> E.Html) E.Attribute where
  (!) e a = e E.! a

instance Exclamatable (E.Html) Id where
  (!) e id = e E.! A.id (id2v id)
instance Exclamatable (E.Html -> E.Html) Id where
  (!) e id = e E.! A.id (id2v id)

instance Exclamatable (E.Html) [Class] where
  (!) e cs = e E.! A.class_ (E.toValue str)
    where str = TL.unwords $ Pr.map (static . unClass) cs
instance Exclamatable (E.Html -> E.Html) [Class] where
  (!) e cs = e E.! A.class_ (E.toValue str)
    where str = TL.unwords $ Pr.map (static . unClass) cs

instance Exclamatable (E.Html) Class where
  (!) e c = e E.! A.class_ (c2v c)
instance Exclamatable (E.Html -> E.Html) Class where
  (!) e c = e E.! A.class_ (c2v c)

id2v = E.toValue . static . unId
c2v = E.toValue . static . unClass
