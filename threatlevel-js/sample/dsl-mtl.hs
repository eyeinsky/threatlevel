module Tmp where

import Common.Prelude
import JS.Syntax
import JS.DSL.Syntax
import JS.DSL.MTL as JS

import Render

test' :: forall m . JS m => m ()
test' = void $ do

  a <- var "a"
  b <- let_ "b"
  c <- const "c"

  a .= b

  comment "jee"

  ifelse (lit True) (do
    d <- let_ "d"
    b .= d)
    (do
    e <- let_ "e"
    c .= e)

  return_ $ lit 1

  tryCatchFinally
    (throw $ lit "error")
    (\e -> a .= e)
    (throw $ lit "finally")

  while (lit 2 .> 1) $ do
    bare $ call1 (ex "console" !. "log") a

  forIn (lit 1) $ \x -> do
    bare $ call1 (ex "console" !. "log") x

  JS.forOf (lit 1) $ \x -> do
    bare $ call1 (ex "console" !. "log") x

  newf $ \(a :: Expr a) (b :: Expr b) -> do
    bare $ call1 (ex "console" !. "log") a
    bare $ call1 (ex "console" !. "log") b

--  f3 @m $ \(a :: Expr a) -> return @m 4


  a .+= b
  a ./= b

  return_ $ lit 1
  return ()

main = let
  renderConf = Indent 2
  in printRender renderConf (test' :: MonoJS ())

-- hot3 = CSS.runEmpty $ class_ $ background "blue"
