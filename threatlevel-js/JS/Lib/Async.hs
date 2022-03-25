module JS.Lib.Async where

import Prelude
import JS

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
