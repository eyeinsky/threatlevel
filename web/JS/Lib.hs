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
   retrn $ ternary (m .=== Null) (lit 1) (m !. "length" + lit 1)

repeat = \ n c -> do
   res <- new $ lit ""
   i <- new $ lit 0
   for (i .< n) $ do res .= res + c; i .= i + lit 1;
   retrn res

round = \ n p -> do
   tens <- new $ p * lit 10
   retrn $ nearestInt (n * tens) / tens

nf = \ n places -> do
   repeat <- lib $ newf repeat
   x <- new $ JS.split (toString n) (lit ".")
   x1 <- new $ x !- 1
   y <- new $ ternary (x1 .=== Undefined) (call repeat [places, lit "0"]) x1
   retrn $ (x!-0) -- .+ lit "." .+ call (y !. "slice") [lit 0, places]

-- * Object

setAttr k v o = call (ex "Object" !. "assign") [ex "{}", Cast o, o1]
  where o1 = pair k v

pair :: Expr a -> Expr b -> Expr c
pair k v = Lit $ Object [(Right $ Cast k, Cast v)]

-- * Array

iterArray :: Expr [a] -> (Expr Int -> M r b) -> M r ()
iterArray arr f = do
  ix <- new 0
  length <- new $ arr !. "length"
  while (ix .< length) $ do
    f ix
    ix .+= 1
