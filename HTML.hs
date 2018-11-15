module HTML
  ( module HTML
  , module HTML.Core
  , module Render
  , module DOM.Core
  , module XML
  ) where

import Control.Monad.Writer
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Lens (utf8)

import qualified Network.HTTP.Types as WT

import Pr hiding (head)
import XML
import Render hiding (Conf)
import DOM.Core
import HTML.Core hiding ((!), M, map, embed, input)
import qualified HTML.Core as Core
import HTTP.Response (utf8textHdr)
import HTML.Core hiding ((!), M, map, embed)

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

placeholder :: TL.Text -> Attribute
placeholder = Custom "placeholder"

metaNC :: TL.Text -> TL.Text -> Html
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
