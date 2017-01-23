module Web.HTML.Shorthands where

import Prelude2
import qualified Data.Text.Lazy as TL
import Control.Monad.Writer

import Language.Haskell.TH

import Web.HTML.Core
import TH

concat <$> mapM (mk [t|HTMLM ()|]) ["div", "span", "a", "form", "h1", "ul", "li", "svg"]
concat <$> mapM (mkAttr 'Custom [t|Attribute|]) ["href"]

id_ :: Id -> Attribute
id_ id = AttrId id :: Attribute

cls_ :: [Class] -> Attribute
cls_ cs = AttrClass cs
