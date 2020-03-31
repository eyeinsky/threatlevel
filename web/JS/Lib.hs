module JS.Lib
  ( module JS.Lib.Sleep
  , module JS.Lib.Async
  , module JS.Lib
  , module JS.DSL
  ) where

import JS.Lib.Sleep
import JS.Lib.Async

import X.Prelude
import qualified Prelude as P
import JS
import JS.DSL (not)

-- * Text

wn = \ str -> do
   m <- const $ call (str !. "match") [ call (ex "new RegExp") ["\\s", "g"] ]
   retrn $ ternary (m .=== Null) (lit 1) (m !. "length" + lit 1)

repeat = \ n c -> do
   res <- let_ $ lit ""
   i <- let_ $ lit 0
   for (i .< n) $ do res .= res + c; i .= i + lit 1;
   retrn res

round = \ n p -> do
   tens <- const $ p * lit 10
   retrn $ nearestInt (n * tens) P./ tens

nf = \ n _ -> do
   -- repeat <- lib $ newf repeat
   x <- const $ JS.split (toString n) (lit ".")
   -- x1 <- new $ x !- 1
   -- y <- new $ ternary (x1 .=== Undefined) (call repeat [places, lit "0"]) x1
   retrn $ (x!-0) -- .+ lit "." .+ call (y !. "slice") [lit 0, places]

-- * Object

setAttr k v o = call (ex "Object" !. "assign") [ex "{}", Cast o, o1]
  where o1 = pair k v

pair :: Expr a -> Expr b -> Expr c
pair k v = Lit $ Object [(Right $ Cast k, Cast v)]

-- * Array

iterArray :: Expr [a] -> (Expr Int -> M r b) -> M r ()
iterArray arr f = do
  ix <- let_ 0
  length <- const $ arr !. "length"
  while (ix .< length) $ do
    _ <- f ix
    ix .+= 1
