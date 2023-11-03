module JS.API where

import Prelude
import JS.DSL
import qualified Data.Text as TS

-- * Async

data JobId

timeout :: ToExpr a => TS.Text -> Expr b -> a -> Expr c
timeout name f ms = call (ex name) [ Cast f, lit ms]

setInterval :: Expr a -> Int -> Expr JobId
setInterval = timeout "setInterval"

clearInterval :: Expr JobId -> Expr ()
clearInterval id = call1 (ex "clearInterval") id

setTimeout :: Expr a -> Int -> Expr JobId
setTimeout = timeout "setTimeout"

clearTimeout :: Expr JobId -> Expr ()
clearTimeout id = call1 (ex "clearTimeout") id
