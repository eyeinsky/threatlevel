module JS.BuiltIns.Array where

import X.Prelude hiding (length)
import JS.DSL

-- ** Mutating

push :: Expr a -> Expr [a] -> Expr c
push a as = call1 (as !. "push") a

pop :: Expr [a] -> Expr a
pop as = call0 (as !. "pop")

join :: Expr String -> Expr [a] -> Expr b
join str arr = call1 (arr !. "join") str

slice :: Expr Double -> Expr Double -> Expr a -> Expr a -- Expr (Array (Expr a)) -> Expr (Array (Expr a))
slice a b s = call (s !. "slice") [a, b]

-- ** Pure

length :: Expr [a] -> Expr Int
length as = as !. "length"

last :: Expr [a] -> Expr a
last as = as .! (length as - lit 1)

indexOf :: Expr [a] -> Expr a -> Expr Int
indexOf as a = call1 (as !. "indexOf") a

splice :: Expr [a] -> Expr Int -> Expr Int -> Expr [a]
splice as a b = call (as !. "splice") [a, b]

concat :: Expr a -> Expr a -> Expr [a]
concat a b = call1 (b !. "concat") a

-- * Helpers

iterArray :: Expr [a] -> (Expr Int -> M r b) -> M r ()
iterArray arr f = do
  ix <- let_ 0
  length <- const $ arr !. "length"
  while (ix .< length) $ do
    _ <- f ix
    ix .+= 1
