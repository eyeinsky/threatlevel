module JS.DSL.MTL.Effect
  ( module JS.DSL.Core
  , module JS.DSL.MTL.Effect
  ) where

import Common.Prelude hiding (next)
import JS.Syntax as Syntax
import JS.DSL.Core

-- * Effect

class (Monad m) => JS m where
  stm :: Syntax.Statement () -> m ()
  freshName :: m Name
  bind :: (Name -> Expr a -> Statement ()) -> Expr a -> m (Expr a)
  execSub :: m a -> m (a, Code_)
  execFunc :: forall f . Function f m => f -> m (Ret f m)

execSub_ :: JS m => m a -> m Code_
execSub_ m = execSub m <&> snd

execFuncUntyped :: forall f m . JS m => Function f m => f -> m RetUntyped
execFuncUntyped f = coerce <$> execFunc @m @f f

funcLet
  :: forall m f . (Function f m, JS m)
  => FuncConstr () (FunctionType f m) -> f -> m (Expr (FunctionType f m))
funcLet syntax f = do
  (args, body) <- execFuncUntyped f
  bind Let $ syntax Nothing args body

-- * Function syntax

type FunctionType :: Type -> (Type -> Type) -> Type
type family FunctionType f m where
   FunctionType (Expr a -> f) m = a -> FunctionType f m
   FunctionType (m a) m = a

newtype Tagged a b = Tagged b
type RetUntyped = ([Name], Code_)
type Ret f m = Tagged (FunctionType f m) RetUntyped

class Monad m => Function (f :: Type) (m :: Type -> Type) where
  c2 :: JS m => f -> [Name] -> m (Ret f m)
instance (Function f m, FunctionType (Expr a -> f) m ~ (a -> FunctionType f m)) => Function (Expr a -> f) m where
  c2 f args = do
    name <- freshName
    let f' = f $ EName name :: f
    coerce <$> c2 f' (name : args)
instance {-# INCOHERENT #-} (m0 ~ m, Monad m) => Function (m0 a) m where
  c2 f args = do
    body <- execSub_ f
    return $ Tagged (reverse args, body)

-- * Convert typed expressions to functions on expressions

-- | Example: @Expr (a -> b -> c)@ to @Expr a -> Expr b -> Expr c@

type family Convert x where
  Convert (Expr (a -> b)) = Expr a -> Convert (Expr b)
  Convert (Expr b) = Expr b
class Back a where
  convert :: [Expr ()] -> a -> Convert a
instance Back (Expr b) => Back (Expr (a -> b)) where
  convert args f arg = convert (Cast arg : args) (Cast f :: Expr b)
instance {-# OVERLAPPABLE #-} (Expr a ~ Convert (Expr a)) => Back (Expr a) where
  convert args f = call f $ reverse args
