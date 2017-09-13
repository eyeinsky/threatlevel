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


-- * Tag

data Html5
type HTML = XML Html5 AttributeSet

-- ** Monadic dsl

type Html = Writer [HTML] ()

declareFields [d|
  data Document = Document
    { documentHead :: Html
    , documentBody :: Html
    }
  |]
