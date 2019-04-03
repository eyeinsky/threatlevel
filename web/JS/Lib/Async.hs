module JS.Lib.Async where

import Prelude2 hiding ((.=), (.>))
import JS

asyncCountdown :: Int -> Int -> Expr () -> Expr () -> M r ()
asyncCountdown i ticks f g = do
  ticks' <- new $ ulit ticks
  countdownId <- new Null
  go <- block $ do
    ifelse (ticks' .> ulit 0)
      (do ticks' .= ticks' - 1
          bare $ call0 f)
      (do bare $ clearInterval countdownId
          bare $ call0 g)
  (countdownId .=) =<<$ new $ setInterval go i
