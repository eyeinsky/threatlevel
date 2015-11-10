module Main where

import Prelude2
import qualified JS_Types as JT
import JS_Monad

main = putStr "Tests!"

test = let
   in do
   return1 :: Expr (Expr JT.String, Proxy JT.NumberI)
      <- newf' "ret1" $ \ (s :: Expr JT.String) ->
         retrn (lit (1::Int) :: Expr JT.NumberI)
   addArgs <- newf' "addArgs" $ \ (a :: Expr JT.NumberI) (b :: Expr JT.NumberI) -> do
      retrn $ a .+ b

   bare $ addArgs `a2` (lit (2::Int), lit (3::Int))
   -- bare $ addArgs -/ lit ("string" :: String) -/ lit (3::Int)     -- errors due to wrong type of argument
   -- bare $ addArgs -/ lit (2::Int) -/ lit (3::Int) -/ lit (4::Int) -- errors due to type mismatch (caused by too many arguments)
