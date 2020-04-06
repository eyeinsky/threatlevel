module JS.API where

import X.Prelude
import JS.DSL

-- * Object

this :: Expr a
this = ex "this"

-- * Bool

true :: Expr Bool
true = ex "true"

false :: Expr Bool
false = ex "false"

-- * Async

data JobId

timeout name f ms = call (ex name) [ Cast f, lit ms]

setInterval :: Expr a -> Int -> Expr JobId
setInterval = timeout "setInterval"

clearInterval :: Expr JobId -> Expr ()
clearInterval id = call1 (ex "clearInterval") id

setTimeout :: Expr a -> Int -> Expr JobId
setTimeout = timeout "setTimeout"

clearTimeout :: Expr JobId -> Expr ()
clearTimeout id = call1 (ex "clearTimeout") id
