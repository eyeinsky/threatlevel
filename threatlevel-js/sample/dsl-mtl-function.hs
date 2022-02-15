{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE QuantifiedConstraints #-}
import Common.Prelude
import JS.Syntax
import JS.DSL.Syntax
import JS.DSL.MTL.Effect as JS
import JS.DSL.MTL as JS


import Render

-- * F1 Test

-- | A function
f1FunPoly :: JS m => Expr a -> Expr a -> m ()
f1FunPoly a b = do
  ret <- bind Let $ call (ex "console" !. "log") [a, b]
  stm $ Return ret

-- | Monomorphize
f1FunMono0 = f1FunPoly :: Expr a -> Expr a -> MonoJS ()
f1FunMono1 = f1FunPoly @MonoJS

-- | Usage
f1Test1 = runPretty $ c1 f1FunPoly []
f1Test2 = runPretty $ c1 f1FunMono0 []
f1Test3 = runPretty $ c1 f1FunMono1 []

-- | Code with function
f1Test :: forall m . (JS m, MonadFor (m ()) ~ m, MonadFor (m (Expr Int)) ~ m) => m ()
f1Test = do
  stm $ BareExpr $ call (ex "console" !. "log") [1, 2]

  f <- f1 $ \a b -> f1FunPoly a b
  stm $ BareExpr $ call1 f "calling with argument"

  g <- f1 $ \(a :: Expr Int) (b :: Expr b) -> do
    JS.forOf (lit [a, b]) $ \x -> do
      bare $ call1 (ex "console" !. "log") x
    return a

  h <- f2 AnonFunc $ \(a :: Expr a) (b :: Expr b) -> do
    stm $ BareExpr $ call (ex "console" !. "log") [1, 2]

  stm $ BareExpr $ call g [1, 2]

f1Main :: IO ()
f1Main = printRender (Indent 2) (f1Test @MonoJS)

-- * F2 Test

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
