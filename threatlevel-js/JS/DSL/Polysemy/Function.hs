{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy.Function where

import Common.Prelude hiding (next)
import JS.Syntax as Syntax
import JS.DSL.Polysemy.Core

import Polysemy hiding (Final, run)
import Polysemy.Writer qualified as Polysemy
import Polysemy.State qualified as Polysemy

-- * Typed functions

-- | @funcLit@ takes a literal haskell function, feeds it new names
-- used in the function body, and returns the generated names -- to be
-- used in AST of function definition.
class Function a where
   type FunctionType a
   type ReturnType a
   funcLit :: a -> M (ReturnType a) [Name]

instance Function (M r a) where
   type FunctionType (M r a) = r
   type ReturnType (M r a) = r
   funcLit = ($> [])
instance Function (Expr a) where
   type FunctionType (Expr a) = a
   type ReturnType (Expr a) = a
   funcLit :: Expr a -> M a [Name]
   funcLit e = write @a (Return $ Cast e) $> []
instance (Function b) => Function (Expr a -> b) where
   type FunctionType (Expr a -> b) = a -> FunctionType b
   type ReturnType (Expr a -> b) = ReturnType b
   funcLit f = do
      arg <- next
      args <- funcLit (f $ EName $ arg)
      return (arg : args)

-- | The convert/back combo turns typed function expressions to actual
-- haskell function calls.
type family Convert x where
  Convert (Expr (a -> b)) = Expr a -> Convert (Expr b)
  Convert (Expr b) = Expr b
class Back a where
  convert :: [Expr ()] -> a -> Convert a
instance Back (Expr b) => Back (Expr (a -> b)) where
  convert args f arg = convert (Cast arg : args) (Cast f :: Expr b)
instance {-# OVERLAPPABLE #-} (Expr a ~ Convert (Expr a)) => Back (Expr a) where
  convert args f = call f $ reverse args

-- * Helpers on top of @Function@ class

-- | Shorthand for clearer type signature. One of @AnonFunc@,
-- @Generator@, @Async@
type FuncConstr f = Maybe Name -> [Name] -> Code (ReturnType f) -> Expr (FunctionType f)

-- | Create function, getting state and reader from enclosing monad.
func :: forall r f . Function f => FuncConstr f -> f -> M r (Expr (FunctionType f))
func constr f = do
  env <- askEnv
  (a, new) <- funcPrim env constr <$> getState <*> pure f
  putState new *> pure a
  where
    -- | Create function from a literal: provide JSM state and reader
    funcPrim
      :: Function a
      => Syntax.Conf -> FuncConstr a -> State -> a -> (Expr (FunctionType a), State)
    funcPrim env constr (State fresh used lib) fexp = (constr Nothing args code, s1)
       where
         ((args, code), s1) = run env lib used fresh (funcLit fexp)


-- | Return formal arguments and
-- bla :: forall r m f . Function f => () -> Poly r m ([Name], Code (ReturnType f))
-- bla () = do
--  env <- askEnv
--  State fresh used lib <- getState
--  let ((args, code), new) = run env lib used fresh undefined -- (funcLit fexp)
--  putState new
  -- return (args, code)
--  undefined

type C' m =
  ( Polysemy.State Idents `Member` m
  )

class Function' f r where
  type FunctionType' f r :: k
  funcLit' :: forall m . C' m => f -> Sem m [Name]

instance C' m => Function' (Sem m a) r where
  type FunctionType' (Sem m a) r = r
  funcLit' _ = pure []

instance (Function' f r) => Function' (Expr a -> f) r where
  type FunctionType' (Expr a -> f) r = a -> FunctionType' f r
  funcLit' :: C' m => (Expr a -> f) -> Sem m [Name]
  funcLit' f = do
    arg <- next
    args <- funcLit' @f @r (f $ EName arg)
    return (arg : args)

type FunctionSyntax' f r = Maybe Name -> [Name] -> Code () -> Expr (FunctionType' f r)

-- | Create function, getting state and reader from enclosing monad.
func'
  :: forall r m f
   . (Function' f r)
  => f
  -> FunctionSyntax' f r
  -> Poly r m (Expr (FunctionType' f r))
func' fLit syntax = do
  let body = funcLit' @f @r @m fLit :: Sem m [Name]
  args <- body
  let fSyntax = syntax Nothing args (undefined :: Code ()) :: Expr (FunctionType' f r)
  return fSyntax
