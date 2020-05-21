module JS.DSL.MTL where

import X.Prelude hiding (Empty, State, Const)
import qualified Data.Text as TS
import qualified Data.Set as S

import qualified Identifiers as IS
import JS.DSL.Identifiers
import JS.Syntax hiding (Conf)
import qualified JS.Syntax

-- * Monad

type Idents = [TS.Text]

declareFields [d|
  data State = State
    { stateIdents :: Idents
    , stateNamedVars :: S.Set TS.Text
    , stateLibrary :: S.Set Int
    }
  |]

instance Default State where
  def = State identifiers S.empty S.empty

type M r a = WriterT (Code r) (StateT State Identity) a

run :: State -> M r a -> ((a, Code r), State)
run s m = id . st . wr $ m
   where id = runIdentity
         st = flip runStateT s
         wr = runWriterT

write :: Statement r -> M r ()
write stm = tell [stm]

-- * Core API

next :: M r Name
next = do
  name :: TS.Text <- IS.next idents
  set :: S.Set TS.Text <- gets (^.namedVars)
  if S.member name set
    then next
    else return $ Name name

pushNamedExpr :: TS.Text -> Expr a -> M r TS.Text
pushNamedExpr n _ = do
   modify (namedVars %~ S.insert n)
   return n

{- | Evaluate JSM code to @Code r@

     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: M sub a -> M parent (Code sub)
mkCode mcode = do
  (w, s1) <- fromNext <$> get <*> pure mcode
  put s1 *> pure w
  where
    fromNext :: State -> M r t -> (Code r, State)
    fromNext s0 m = (w, s1)
      where
        ((_, w), s1) = run s0 m
