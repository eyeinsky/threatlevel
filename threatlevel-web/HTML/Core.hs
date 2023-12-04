{-# OPTIONS_GHC -Wno-missing-signatures #-}
module HTML.Core
  ( module HTML.Core
  , module DOM.Core
  ) where

import Common.Prelude hiding (id)
import Control.Monad.Writer

import DOM.Core hiding (Document)
import XML
import XML.TH
import HTML.Paste

data Html5
type HTML c = XML Html5 AttributeSet c
type HtmlM = Writer [HTML Both]
type Html = HtmlM ()

-- * Attributes

concat <$> mapM (mkAttr 'Custom [t|Attribute|]) ["href", "type", "rel", "http-equiv", "content", "action", "method", "crossorigin", "integrity", "src", "scope", "for"]
concat <$> mapM (mk [t|Html|]) tags
