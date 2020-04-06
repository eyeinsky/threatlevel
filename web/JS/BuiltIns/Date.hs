module JS.BuiltIns.Date where

import X.Prelude
import JS.DSL

data Date

dateConstructor = ex "Date"

dateObj = New dateConstructor

dateFrom :: Expr a -> Expr Date
dateFrom = call1 dateObj

date :: Expr Date
date = call0 dateObj

now :: Expr Int
now = call0 $ dateConstructor !. "now"

getTime :: Expr Int
getTime = call0 $ dateConstructor !. "getTime"
