module Web.Lib.JS_CSS where

import Common.Prelude
import DOM.Core qualified as DOM
import JS
import CSS
import Render

-- ** JS + CSS syntax

jsClassName :: Class -> Expr String
jsClassName (Class txt) = lit txt

jsIdName :: Id -> Expr String
jsIdName (Id txt) = lit txt
