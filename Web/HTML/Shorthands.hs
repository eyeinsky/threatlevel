module Web.HTML.Shorthands where

import Prelude2
import qualified Data.Text.Lazy as TL
import Control.Monad.Writer

import Language.Haskell.TH

import Web.HTML.Core
import TH

import Web.HTML.Shorthands.Paste

concat <$> mapM (mk [t|HTMLM ()|]) tags

concat <$> mapM (mkAttr 'Custom [t|Attribute|]) ["href", "type", "rel", "http-equiv", "content" ]

id_ :: Id -> Attribute
id_ id = AttrId id :: Attribute

cls_ :: [Class] -> Attribute
cls_ cs = AttrClass cs
