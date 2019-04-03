module JS.Examples where

import Prelude
import Data.Proxy
import JS

-- | Using 'lib' function f is defined only once.
a = let
    f = lib $ newf $ \ a -> bare $ call0 (a !. "blaa")
  in do
  f1 <- f
  f2 <- f
  f3 <- f
  bare $ call1 f1 (ulit 1)
  bare $ call1 f2 (ulit 1)
  bare $ call1 f3 (ulit 1)
  retrn Null

b = do
  f <- newf $ \ a b c -> retrn $ a .+ b .+ c
  g <- new $ f -/ ulit 1 -- -/ ulit 2

  while (1 .== 2) $ do
    bare $ call0 (g -/ 2 -/ 3)
  return g
