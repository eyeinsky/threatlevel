module JS.Lib.Sleep where

import Prelude
import JS

mkSleep :: (Expr Double -> Expr Double -> Expr Double) -> Expr Double -> M r ()
mkSleep f s = do
  e <- new $ getTime .+ (f s $ lit 1000)
  while (getTime .<= e) empty

sleep :: Expr Double -> M r ()
sleep = mkSleep (.*)

usleep :: Expr Double -> M r ()
usleep = mkSleep (./)
