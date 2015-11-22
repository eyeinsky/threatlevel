module JS_Blaze () where

import Prelude2
import qualified Text.Blaze.Html5 as E
import qualified JS as J

_ASTtoText = J.runPrint . J.ev
instance E.ToValue (J.Expr a) where
   toValue = E.toValue . _ASTtoText
   preEscapedToValue = E.preEscapedToValue . _ASTtoText
instance E.ToValue (J.Code a) where
   toValue = E.toValue . _ASTtoText
   preEscapedToValue = E.preEscapedToValue . _ASTtoText
instance E.ToMarkup (J.Expr a) where
   toMarkup = E.toMarkup . _ASTtoText
   preEscapedToMarkup = E.preEscapedToMarkup . _ASTtoText
instance E.ToMarkup (J.Code a) where
   toMarkup = E.toMarkup . _ASTtoText
   preEscapedToMarkup = E.preEscapedToMarkup . _ASTtoText
