module JS.DSL.Monad where

import X.Prelude hiding (Empty, State, Const)
import qualified Data.Text as TS
import qualified Data.Set as S

import qualified Identifiers as IS
import JS.DSL.Identifiers
import JS.Syntax hiding (Conf)
import qualified JS.Syntax

-- * Monad

type Idents = [TS.Text]
type W r = Code r

declareFields [d|
  data Conf = Conf
    { confNamedVars :: Bool
    , confRenderConf :: JS.Syntax.Conf
    } deriving (Eq, Show, Read)
  |]

declareFields [d|
  data State = State
    { stateIdents :: Idents
    , stateNamedVars :: S.Set TS.Text
    , stateLibrary :: S.Set Int
    }
  |]

class HasConf s a | s -> a where
  conf :: Lens' s a
  {-# MINIMAL conf #-}

instance HasConf Conf Conf where
  conf = id

instance Default State where
  def = State identifiers S.empty S.empty

instance Default Conf where
  def = Conf True (JS.Syntax.Indent 2)

type M r = WriterT (W r) (StateT State (ReaderT Conf Identity))

runM :: Conf -> State -> M r a -> ((a, W r), State)
runM r s = id . rd . st . wr
   where id = runIdentity
         st = flip runStateT s
         wr = runWriterT
         rd = flip runReaderT r

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

{- | Evaluate JSM code to Code aka W r aka [Statement]
     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: M sub a -> M parent (W sub)
mkCode mcode = do
  (w, s1) <- fromNext <$> ask <*> get <*> pure mcode
  put s1 *> pure w
  where
    fromNext :: Conf -> State -> M r t -> (W r, State)
    fromNext b s0 m = (w, s1)
      where
        ((_, w), s1) = runM b s0 m
