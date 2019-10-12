module HTML.Core
  ( module HTML.Core
  , module DOM.Core
  ) where

import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Control.Monad.Writer

import Render

import X.Prelude hiding (id)
import qualified JS
import DOM.Core hiding (Document)
import DOM.Event
import XML
import TH
import HTML.Paste
import URL

data Html5
type HTML c = XML Html5 AttributeSet c
type Html = Writer [HTML Both] ()

-- * Attributes

concat <$> mapM (mkAttr 'Custom [t|Attribute|]) ["href", "type", "rel", "http-equiv", "content", "action", "method", "crossorigin", "integrity", "src", "scope", "for"]
concat <$> mapM (mk [t|Html|]) tags
