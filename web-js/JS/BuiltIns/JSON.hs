module JS.BuiltIns.JSON where

import Prelude
import JS.DSL

jSON :: Expr a
jSON = ex "JSON"

fromJSON :: Expr String -> Expr a
fromJSON = call1 (jSON !. "parse")

toJSON :: Expr a -> Expr String
toJSON = stringify

stringify :: Expr a -> Expr String
stringify a = call1 (jSON !. "stringify") a
