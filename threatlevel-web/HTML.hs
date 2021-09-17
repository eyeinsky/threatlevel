module HTML
  ( module HTML
  , module HTML.Core
  , module Render
  , module DOM.Core
  , module XML
  ) where

import Control.Monad.Writer
import qualified Data.Text as TS

import X.Prelude hiding (head)
import XML hiding (Raw)
import Render hiding (Conf)
import DOM.Core hiding (Document, Class, Id)
import HTML.Core hiding (map, embed, input, link, Class, Id)
import qualified HTML.Core as Core

declareFields [d|
  data Document = Document
    { documentHead' :: Html
    , documentBody' :: Html
    }
  |]

-- * Response

instance Render Document where
  renderM (Document h b) = tl
    where
      html' = html (head h >> b)
      tl = ("<!DOCTYPE html>" <>) <$> (renderM html')

-- * Shorthands

input :: Html
input = Core.input $ pure ()

checkbox :: Html
checkbox = input ! type_ "checkbox"

link :: Html
link = Core.link (pure ())

placeholder :: Value -> Attribute
placeholder = Custom "placeholder"

metaNC :: Value -> Value -> Html
metaNC name content = meta
  ! Custom "name" name
  ! Custom "content" content
  $ pure ()

_blank :: Attribute
_blank = Custom "target" "_blank"

emptyFavicon :: Html
emptyFavicon = link
  ! rel "icon"
  ! href "data:;base64,iVBORw0KGgo="

required :: Attribute
required = Boolean "required" True

readOnly :: Attribute
readOnly = Boolean "readOnly" True

disabled :: Attribute
disabled = Boolean "disabled" True

aria :: TS.Text -> Value -> Attribute
aria name value = Custom ("aria-" <> name) value

--

cssTag :: Html -> Html
cssTag = style ! type_ "text/css"

jsTag :: Html -> Html
jsTag = script ! type_ "text/javascript"

favicon :: Value -> Html
favicon adr = link
  ! rel "shortcut icon"
  ! type_ "image/x-icon"
  ! href adr

property :: Value -> Value -> Html
property name value = meta
  ! Custom "property" name
  ! Custom "content" value
  $ pure ()

og :: TS.Text -> Value -> Html
og name value = property (Static $ "og:" <> name) value

docBody :: Html -> Document
docBody = Document (return ())
