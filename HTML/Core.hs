module HTML.Core
  ( module HTML.Core
  , module DOM.Core
  ) where

import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Control.Monad.Writer

import Render hiding (concat)

import Pr hiding (id)
import qualified JS
import DOM.Core
import DOM.Event
import XML
import TH
import HTML.Paste

data Html5
type HTML c = XML Html5 AttributeSet c
type Html = Writer [HTML Both] ()

declareFields [d|
  data Document = Document
    { documentHead' :: Html
    , documentBody' :: Html
    }
  |]

-- * Attributes

concat <$> mapM (mkAttr 'Custom [t|Attribute|]) ["href", "type", "rel", "http-equiv", "content" ]
concat <$> mapM (mk [t|Html|]) tags

--

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

property :: TL.Text -> TL.Text -> Html
property name value = meta ! Custom "property" name ! Custom "content" value $ pure ()

og :: TL.Text -> TL.Text -> Html
og name value = property ("og:" <> name) value

docBody :: Html -> Document
docBody = Document (return ())

-- | Additional shorthands

checkbox :: Html
checkbox = input ! type_ "checkbox" $ pure ()

placeholder :: TL.Text -> Attribute
placeholder = Custom "placeholder"
