{-# OPTIONS_GHC -Wno-missing-signatures #-}
module HTML.Core
  ( module HTML.Core
  , module DOM.Core
  ) where

import Control.Monad.Writer

import X.Prelude hiding (id)
import DOM.Core hiding (Document)
import XML
import TH
import HTML.Paste

data Html5
type HTML c = XML Html5 AttributeSet c
type Html = Writer [HTML Both] ()

-- * Attributes

concat <$> mapM (mkAttr 'Custom [t|Attribute|]) ["href", "type", "rel", "http-equiv", "content", "action", "method", "crossorigin", "integrity", "src", "scope", "for"]
concat <$> mapM (mk [t|Html|]) tags
