module HTML
  ( module HTML
  , module HTML.Core
  , module Render
  , module DOM.Core
  , module XML
  ) where

import Control.Monad.Writer
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Lens (utf8)

import qualified Network.HTTP.Types as WT

import X.Prelude hiding (head)
import XML hiding (Raw)
import Render hiding (Conf)
import DOM.Core hiding (Document)
import HTML.Core hiding ((!), M, map, embed, input)
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

placeholder :: TS.Text -> Attribute
placeholder = Custom "placeholder"

metaNC :: TS.Text -> TS.Text -> Html
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
  $ pure ()

required :: Attribute
required = Custom "required" ""

--

cssTag :: Html -> Html
cssTag = style ! type_ "text/css"

jsTag :: Html -> Html
jsTag = script ! type_ "text/javascript"

favicon :: TS.Text -> Html
favicon adr = link
  ! rel "shortcut icon"
  ! type_ "image/x-icon"
  ! href adr
  $ pure ()

property :: TS.Text -> TS.Text -> Html
property name value = meta ! Custom "property" name ! Custom "content" value $ pure ()

og :: TS.Text -> TS.Text -> Html
og name value = property ("og:" <> name) value

docBody :: Html -> Document
docBody = Document (return ())
