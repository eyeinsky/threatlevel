{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy.Function where

import Common.Prelude hiding (next)
import Polysemy

import JS.Syntax as Syntax
import JS.DSL.Polysemy.Effect

-- * Typed functions

-- | Shorthand for clearer type signature. One of @AnonFunc@,
-- @Generator@, @Async@
-- type FunctionSyntax f = FuncConstr (ReturnType f) (FunctionType f)

-- type FunctionType a
-- type ReturnType a

-- * Function

-- | @funcLit@ takes a literal haskell function, feeds it new names
-- used in the function body, and returns the generated names -- to be
-- used in AST of function definition.
class Function f where
  type Row (f :: Type) :: EffectRow
  type Return (f :: Type) :: Type
  type ApplyType (f :: Type) :: Type
  funcLit
    :: Member (JS s) (Row f)
    => f -> Sem (Row f) ([Name], Sem (Row f) (Return f))

instance Function (Sem r a) where
  type Row (Sem r a) = r
  type Return (Sem r a) = a
  type ApplyType (Sem r a) = a
  funcLit fbody = do
    return ([], fbody)

instance
  ( Function f, Row f ~ (JS s : s)
  ) => Function (Expr a -> f) where

  type Row (Expr a -> f) = Row f
  type Return (Expr a -> f) = Return f
  type ApplyType (Expr a -> f) = a -> ApplyType f

  funcLit f = do
    arg <- getFreshIdentifier @s
    let f' = f (EName arg) :: f
    (args, fbody) <- funcLit @f @s f'
    return (arg : args, fbody)

-- | Saturate function @f@ to arguments and body (@fbody@)
getSyntax
  :: forall f s r
   . (Function f, Row f ~ r, Member (JS s) r, r ~ (JS s : s))
  => (FuncConstr () (ApplyType f))
  -> f -> Sem r (Expr (ApplyType f))
getSyntax constr f = do
  (args, fbody :: Sem r (Return f)) <- funcLit @f @s f
  code_ :: Code_ <- generateCode fbody
  let fexpr = constr Nothing args code_ :: Expr (ApplyType f)
  return fexpr

-- -- | The convert/back combo turns typed function expressions to actual
-- -- haskell function calls.
-- type family Convert x where
--   Convert (Expr (a -> b)) = Expr a -> Convert (Expr b)
--   Convert (Expr b) = Expr b
-- class Back a where
--   convert :: [Expr ()] -> a -> Convert a
-- instance Back (Expr b) => Back (Expr (a -> b)) where
--   convert args f arg = convert (Cast arg : args) (Cast f :: Expr b)
-- instance {-# OVERLAPPABLE #-} (Expr a ~ Convert (Expr a)) => Back (Expr a) where
--   convert args f = call f $ reverse args

-- * Helpers on top of @Function@ class


-- -- | Create function, getting state and reader from enclosing monad.
-- func
--   :: forall r f . (Member (JS r) r, Function f)
--   => FuncConstr' f -> f -> Sem r (Expr (FunctionType f))
-- func constr f = do
--   let fbody = funcLit @f @r f :: Sem r [Name]
  -- ^ I think the @e@ means both current scope and function need to
  -- have the same return type?

--  defineFunction fbody

--   env <- askEnv
--   state <- getState
--   let (a, new) = funcPrim env constr state f
--   putState new *> pure a
--   where
--     -- | Create function from a literal: provide JSM state and reader
--     funcPrim
--       :: Function a
--       => Syntax.Conf -> FuncConstr a -> State -> a -> (Expr (FunctionType a), State)
--     funcPrim env constr (State fresh used lib) fexp = (constr Nothing args code, s1)
--        where
--          ((args, code), s1) = run env lib used fresh (funcLit fexp)
--  undefined

-- | Return formal arguments and
-- bla :: forall r m f . Function f => () -> Poly r m ([Name], Code (ReturnType f))
-- bla () = do
--  env <- askEnv
--  State fresh used lib <- getState
--  let ((args, code), new) = run env lib used fresh undefined -- (funcLit fexp)
--  putState new
  -- return (args, code)
--  undefined
