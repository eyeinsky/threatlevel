module JS.DSL.Polysemy.Function where

import Common.Prelude hiding (Type, next)
import JS.Syntax as Syntax
import JS.DSL.Polysemy.Core

-- * Typed functions

-- | @funcLit@ takes a literal haskell function, feeds it new names
-- used in the function body, and returns the generated names -- to be
-- used in AST of function definition.
class Function a where
   type Type a
   type Final a
   funcLit :: a -> M (Final a) [Name]
instance Function (M r a) where
   type Type (M r a) = r
   type Final (M r a) = r
   funcLit = ($> [])
instance Function (Expr a) where
   type Type (Expr a) = a
   type Final (Expr a) = a
   funcLit :: Expr a -> M a [Name]
   funcLit e = write @a (Return $ Cast e) $> []
instance (Function b) => Function (Expr a -> b) where
   type Type (Expr a -> b) = a -> Type b
   type Final (Expr a -> b) = Final b
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
type FuncConstr f = Maybe Name -> [Name] -> Code (Final f) -> Expr (Type f)

-- | Create function, getting state and reader from enclosing monad.
func :: forall r f . Function f => FuncConstr f -> f -> M r (Expr (Type f))
func constr f = do
  env <- askEnv
  (a, new) <- funcPrim env constr <$> getState <*> pure f
  putState new *> pure a
  where
    -- | Create function from a literal: provide JSM state and reader
    funcPrim
      :: Function a
      => Syntax.Conf -> FuncConstr a -> State -> a -> (Expr (Type a), State)
    funcPrim env constr (State fresh used lib) fexp = (constr Nothing args code, s1)
       where
         ((args, code), s1) = run env lib used fresh (funcLit fexp)


-- | Return formal arguments and
bla :: forall r f . Function f => f -> M r ([Name], Code (Final f))
bla fexp = do
  env <- askEnv
  State fresh used lib <- getState
  let ((args, code), new) = run env lib used fresh (funcLit fexp)
  putState new
  return (args, code)
