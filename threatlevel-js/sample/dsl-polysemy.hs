module Tmp where

import Common.Prelude
import JS.DSL.Polysemy as JS hiding (hot)
import CSS as CSS


test' :: C s ()
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

  log <- newf $ \a b -> do
    bare $ call1 (ex "console" !. "log") a
    bare $ call1 (ex "console" !. "log") b
    logAsync <- async $ \e ->
      bare $ call1 (ex "console" !. "log") e
    return_ logAsync

  logGen <- generator $ \e ->
    bare $ call1 (ex "console" !. "log") e

  bare $ call1 log 1
  bare $ call1 logGen 1

  a .+= b
  a ./= b

  retrn $ lit 1
  return ()

hot' = printJS test'

hot3 = CSS.runEmpty $ class_ $ background "blue"
