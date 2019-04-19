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
  bare $ call1 f1 (lit 1)
  bare $ call1 f2 (lit 1)
  bare $ call1 f3 (lit 1)
  retrn Null

b = do
  f <- newf $ \ a b c -> retrn $ a .+ b .+ c
  g <- new $ f -/ lit 1 -- -/ lit 2

  while (1 .== 2) $ do
    bare $ call0 (g -/ 2 -/ 3)
  return g
