module JS.Blaze () where

import Prelude2
import qualified Text.Blaze.Html5 as E
import JS

_ASTtoText = runRender . renderM
instance E.ToValue (Expr a) where
   toValue = E.toValue . _ASTtoText
   preEscapedToValue = E.preEscapedToValue . _ASTtoText
instance E.ToValue (Code a) where
   toValue = E.toValue . _ASTtoText
   preEscapedToValue = E.preEscapedToValue . _ASTtoText
instance E.ToMarkup (Expr a) where
   toMarkup = E.toMarkup . _ASTtoText
   preEscapedToMarkup = E.preEscapedToMarkup . _ASTtoText
instance E.ToMarkup (Code a) where
   toMarkup = E.toMarkup . _ASTtoText
   preEscapedToMarkup = E.preEscapedToMarkup . _ASTtoText
