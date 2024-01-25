module JS.Apis.EventLoop where

import Common.Prelude
import Data.Text qualified as TS
import JS.DSL

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

-- * Extras

asyncCountdown :: JS m => Int -> Int -> Expr () -> Expr () -> m ()
asyncCountdown i ticks f g = do
  ticks' <- let_ $ lit ticks
  countdownId <- let_ Null
  go <- newf $ do
    ifelse (ticks' .> lit 0)
      (do ticks' .= ticks' - 1
          bare $ call0 f)
      (do bare $ clearInterval countdownId
          bare $ call0 g)
  countdownId .= setInterval go i
