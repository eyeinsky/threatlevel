module JS.Lib
  ( module JS.Lib.Sleep
  , module JS.Lib.Async
  , module JS.Lib
  ) where

import JS.Lib.Sleep
import JS.Lib.Async

import Prelude hiding (repeat)
import JS

-- * Text

wn = \ str -> do
   m <- new $ call (str !. "match") [ call (ex "new RegExp") ["\\s", "g"] ]
   retrn $ ternary (m .=== Null) (ulit 1) (m !. "length" .+ ulit 1)

repeat = \ n c -> do
   res <- new $ ulit ""
   i <- new $ ulit 0
   for (i .< n) $ do res .= res .+ c; i .= i .+ ulit 1;
   retrn res

round = \ n p -> do
   tens <- new $ p .* ulit 10
   retrn $ nearestInt (n .* tens) ./ tens

nf = \ n places -> do
   repeat <- lib $ newf repeat
   x <- new $ JS.split (toString n) (ulit ".")
   x1 <- new $ x !- 1
   y <- new $ ternary (x1 .=== Undefined) (call repeat [places, ulit "0"]) x1
   retrn $ (x!-0) -- .+ ulit "." .+ call (y !. "slice") [ulit 0, places]
