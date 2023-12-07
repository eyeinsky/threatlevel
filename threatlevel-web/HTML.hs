module HTML
  ( module HTML
  , module HTML.Core
  , module Render
  , module DOM.Core
  , module XML
  ) where

import Common.Prelude hiding (head)
import Control.Monad.Writer
import Data.Text qualified as TS

import XML hiding (Raw)
import Render hiding (Conf)
import DOM.Core hiding (Document, Class, Id)
import HTML.Core hiding (map, embed, input, link, Class, Id)
import qualified HTML.Core as Core

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
cssTag = Core.style ! type_ "text/css"

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

-- | Embed file as Data URL. Format: https://developer.mozilla.org/en-US/docs/web/http/basics_of_http/data_urls
dataUrl :: Maybe TS.Text -> Maybe TS.Text -> TS.Text -> Value
dataUrl maybeMediaType maybeEncoding data_ = Static $ "data:" <> mediaType <> encoding <> "," <> data_
  where
    mediaType = fromMaybe "" maybeMediaType
    encoding = case maybeEncoding of
      Just encoding' -> ";" <> encoding'
      Nothing -> ""

-- * Document

newtype Document = Document Html

instance Render Document where
  renderM (Document html) =  pure "<!DOCTYPE html>" Render.<+> renderM html
