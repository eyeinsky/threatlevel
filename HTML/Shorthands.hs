module HTML.Shorthands where

import Prelude2
import qualified Data.Text.Lazy as TL
import Control.Monad.Writer

import Language.Haskell.TH

import HTML.Core
import TH

import HTML.Shorthands.Paste

-- * Attributes

concat <$> mapM (mkAttr 'Custom [t|Attribute|]) ["href", "type", "rel", "http-equiv", "content" ]

id_ :: Id -> Attribute
id_ id = AttrId id :: Attribute

cls_ :: [Class] -> Attribute
cls_ cs = AttrClass cs

-- * Elements

concat <$> mapM (mk [t|Html|]) tags

cssTag :: Html -> Html
cssTag = style ! type_ "text/css"

jsTag :: Html -> Html
jsTag = script ! type_ "text/javascript"

favicon :: TL.Text -> Html
favicon adr = link
  ! rel "shortcut icon"
  ! type_ "image/x-icon"
  ! href adr
  $ pure ()

docBody :: Html -> Document
docBody = Document (return ())
