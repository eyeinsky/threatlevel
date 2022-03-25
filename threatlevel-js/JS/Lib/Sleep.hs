module JS.Lib.Sleep where

import Prelude as P hiding (const)
import JS

mkSleep
  :: JS m
  => (Expr Double -> Expr Double -> Expr Double) -> Expr Double -> m ()
mkSleep f s = do
  e <- const $ getTime + (Cast $ f s $ lit 1000)
  while (getTime .<= e) empty

sleep :: JS m => Expr Double -> m ()
sleep d = mkSleep (*) (Cast d)

usleep :: JS m => Expr Double -> m ()
usleep = mkSleep (P./)
