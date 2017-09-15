module HTML.Core
  ( module HTML.Core
  , module DOM.Core
  ) where

import Pr hiding (id)
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import Control.Monad.Writer

import qualified JS

import DOM.Core
import DOM.Event
import XML
import Render


-- * Tag

data Html5
type HTML c = XML Html5 AttributeSet c

-- ** Monadic dsl

type Html = Writer [HTML Both] ()

declareFields [d|
  data Document = Document
    { documentHead :: Html
    , documentBody :: Html
    }
  |]
