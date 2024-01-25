module Web.Lib.JS_CSS where

import Common.Prelude
import DOM.Core qualified as DOM
import JS
import CSS
import Render

jsClassName :: Class -> Expr String
jsClassName (Class txt) = lit txt

jsIdName :: Id -> Expr String
jsIdName (Id txt) = lit txt

instance ToExpr DOM.Id where
  lit = coerce @_ @DOM.Value ^ render' ^ lit

instance ToExpr DOM.Class where
  lit = coerce @_ @DOM.Value ^ render' ^ lit

instance ToExpr SimpleSelector where
  lit = lit . render CSS.Minify

-- | In JS set element's inline style to @declarations@ [api]
inlineStyle :: JS m => Expr tag -> MonoProp () -> m ()
inlineStyle element declarations = do
  forM_ (execMonoProp declarations) $ \(Declaration k v) -> let
    property = tsKebab2camel k
    in element !. "style" !. property .= lit (render CSS.Minify v)
