module Main where

import Prelude2
import JS

main = putStr "Tests!"

test = let
   in do
   return1 :: Expr (Expr JS.String, Proxy JS.NumberI)
      <- newf' "ret1" $ \ (s :: Expr JS.String) ->
         retrn (lit (1::Int) :: Expr JS.NumberI)
   addArgs <- newf' "addArgs" $ \ (a :: Expr JS.NumberI) (b :: Expr JS.NumberI) -> do
      retrn $ a .+ b

   bare $ addArgs `a2` (lit (2::Int), lit (3::Int))
   -- bare $ addArgs -/ lit ("string" :: String) -/ lit (3::Int)     -- errors due to wrong type of argument
   -- bare $ addArgs -/ lit (2::Int) -/ lit (3::Int) -/ lit (4::Int) -- errors due to type mismatch (caused by too many arguments)
