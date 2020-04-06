module JS.BuiltIns.String where

import X.Prelude
import JS.DSL

toString :: Expr a -> Expr String
toString obj = call0 (obj !. "toString")

split :: Expr String -> Expr String -> Expr [String]
split str sep = call1 (str !. "split") sep

trim :: Expr String -> Expr String
trim s = call0 (s !. "trim")
