module JS.API where

import Prelude
import Data.Text qualified as TS
import JS.DSL

-- * Async

data JobId

timeout :: ToExpr a1 => TS.Text -> Expr a2 -> a1 -> Expr c
timeout name f ms = call (ex name) [ Cast f, lit ms]

setInterval :: Expr a -> Int -> Expr JobId
setInterval = timeout "setInterval"

clearInterval :: Expr JobId -> Expr ()
clearInterval id = call1 (ex "clearInterval") id

setTimeout :: Expr a -> Int -> Expr JobId
setTimeout = timeout "setTimeout"

clearTimeout :: Expr JobId -> Expr ()
clearTimeout id = call1 (ex "clearTimeout") id
