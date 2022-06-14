{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE QuantifiedConstraints #-}
import Common.Prelude
import JS.Syntax
import JS.DSL.Syntax
import JS.DSL.MTL.Effect as JS
import JS.DSL.MTL as JS

import Render


f2Test0 :: forall m . JS m => m ()
f2Test0 = stm $ BareExpr $ call (ex "console" !. "log") [1, 2]

f2Main0 :: IO ()
f2Main0 = printRender (Indent 2) (f2Test0 @MonoJS)

f2Poly :: forall m . JS m => Expr () -> Expr () -> m ()
f2Poly a b = do
    JS.forOf (lit [a, b]) $ \x -> do
      bare $ call1 (ex "console" !. "log") x
    return () :: m ()

f2Test :: forall m . JS m => m ()
f2Test = do
  stm $ BareExpr $ call (ex "console" !. "log") [1, 2]
  h <- f2 AnonFunc $ \(a :: Expr a) (b :: Expr a) -> do
    JS.forOf (lit [a, b]) $ \x -> do
      bare $ call1 (ex "console" !. "log") x
    return ()
  stm $ BareExpr $ call h [1, 2]
  return ()

f2Main :: IO ()
f2Main = printRender (Indent 2) (f2Test @MonoJS)

main :: IO ()
main = f2Main
