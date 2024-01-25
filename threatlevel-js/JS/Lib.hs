module JS.Lib
  ( module JS.Lib.Sleep

  , module JS.Lib
  , module JS.DSL
  ) where

import JS.Lib.Sleep

import Prelude hiding (const)
import qualified Prelude as P
import JS
import JS.DSL (not)

-- * Text

wn :: JS m => Expr a -> m ()
wn = \ str -> do
   m <- const $ call (str !. "match") [ call (ex "new RegExp") ["\\s", "g"] ]
   return_ $ ternary (m .=== Null) (lit 1) (m !. "length" + lit 1)

repeat :: JS m => Expr b -> Expr r -> m ()
repeat = \ n c -> do
   res <- let_ $ lit ""
   i <- let_ $ lit 0
   for (i .< n) $ do res .= res + c; i .= i + lit 1;
   return_ res

round :: JS m => Expr r -> Expr r -> m ()
round = \ n p -> do
   tens <- const $ p * lit 10
   return_ $ nearestInt (n * tens) P./ tens

nf :: JS m => Expr a -> p -> m ()
nf = \ n _ -> do
   -- repeat <- lib $ newf repeat
   x <- const $ JS.split (toString n) (lit ".")
   -- x1 <- new $ x !- 1
   -- y <- new $ ternary (x1 .=== Undefined) (call repeat [places, lit "0"]) x1
   return_ $ (x!-0) -- .+ lit "." .+ call (y !. "slice") [lit 0, places]
