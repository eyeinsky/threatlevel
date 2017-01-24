module JS.Blaze () where

import Prelude2
import qualified Text.Blaze.Html5 as E
import JS

instance E.ToValue (Expr a) where
   toValue = E.toValue . render
   preEscapedToValue = E.preEscapedToValue . render
instance E.ToValue (Code a) where
   toValue = E.toValue . render
   preEscapedToValue = E.preEscapedToValue . render
instance E.ToMarkup (Expr a) where
   toMarkup = E.toMarkup . render
   preEscapedToMarkup = E.preEscapedToMarkup . render
instance E.ToMarkup (Code a) where
   toMarkup = E.toMarkup . render
   preEscapedToMarkup = E.preEscapedToMarkup . render
