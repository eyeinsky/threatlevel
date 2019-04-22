{-# LANGUAGE ExtendedDefaultRules #-}
module JS.DSL.Internal
  ( module Web.Browser
  , module JS.DSL.Internal
  ) where

import Prelude2 hiding ((.-), for, (.=), (.>), Empty, Const)

import Data.Default

import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Hashable as H

import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS
import qualified Data.Text.IO as TLIO
import Data.String

import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer
import Control.Monad.Identity

import qualified JS.Render
import JS.Syntax hiding (S, putStrLn, Conf)

import qualified JS.Syntax as JS
import qualified Identifiers as IS
import JS.DSL.Identifiers
import Web.Browser

-- * Monad

type Idents = [TS.Text]
type W r = Code r

declareFields [d|
  data Conf = Conf
    { confBrowser :: Browser
    , confNamedVars :: Bool
    , confRenderConf :: JS.Render.Conf
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

instance {-# OVERLAPS #-} (HasConf c Conf) => HasBrowser c Browser where
  browser = conf.browser


instance Default State where
  def = State identifiers S.empty S.empty

instance Default Conf where
  def = Conf Unknown True (JS.Render.Indent 2)

type M r = WriterT (W r) (StateT State (ReaderT Conf Identity))

runM :: Conf -> State -> M r a -> ((a, W r), State)
runM r s = id . rd . st . wr
   where id = runIdentity
         st = flip runStateT s
         wr = runWriterT
         rd = flip runReaderT r

fromNext :: Conf -> State -> M r t -> (W r, State)
fromNext b s0 m = (w, s1)
  where
    ((_, w), s1) = runM b s0 m

-- * Core API

next :: M r Name
next = do
  name :: TS.Text <- IS.next idents
  set :: S.Set TS.Text <- gets (^.namedVars)
  if S.member name set
    then next
    else return $ Name name

new, let_, const :: Expr a -> M r (Expr a)
new = newPrim VarDef
var = new
let_ = newPrim Let
const = newPrim Const

newPrim :: (Name -> Expr a -> Statement r) -> Expr a -> M r (Expr a)
newPrim kw e = bind kw e =<< next

bind kw expr name = do
   define name $ Cast expr
   return $ Cast $ EName name
   where define name expr = tell [ kw name expr ]

pushNamedExpr :: TS.Text -> Expr a -> M r TS.Text
pushNamedExpr n e = do
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

-- *** Typed
tcall :: (Show a, Args a) => Expr (a, r) -> a -> (Expr r)
tcall f as = TypedFCall f as

-- * Typed functions

-- | Create function from a literal: provide JSM state and reader
funcPrim
  :: Function a
  => (Maybe Name -> [Expr ()] -> Code (Final a) -> Expr (Type a))
  -> Conf -> State -> a -> (Expr (Type a), State)
funcPrim constr r s0 fexp = (constr Nothing args code, s1)
   where
     ((a, _), s1) = runM r s0 (funcLit fexp)
     (args, code) = a

-- | The 'Function' class turns function literals into typed
-- functions.
class Function a where
   type Type a
   type Final a
   funcLit :: a -> M self ([Expr ()], Code (Final a))
instance Function (M r a) where
   type Type (M r a) = r
   type Final (M r a) = r
   funcLit f = ([], ) <$> mkCode f
instance (Function b) => Function (Expr a -> b) where
   type Type (Expr a -> b) = a -> Type b
   type Final (Expr a -> b) = Final b
   funcLit f = do
      arg :: Expr a <- EName <$> next
      (args, b) <- funcLit (f arg)
      return (Cast arg : args, b)

class Apply f a where
   type Result f a
   fapply :: Expr f -> a -> (Expr (Result f a), [Expr ()], Int)
instance Apply (Proxy f) () where
   {- Function is exhausted, start returning. -}
   type Result (Proxy f) () = Proxy f
   fapply f _ = (f, [], 0)
instance Apply fs () => Apply (f, fs) () where
   {- All actual arguments are applied, but the function
      is not fully saturated. Count the remaining arguments
      into an Int. -}
   type Result (f, fs) () = (f, fs)
   fapply f _ = (f, [], 1+i)
      where (_,_,i) = fapply (Cast f :: Expr fs) ()
instance (f ~ a, Apply fs as)
   => Apply (Expr f, fs) (Expr a, as) where
   {- More of both formal and actual arguments to apply. -}
   type Result (Expr f, fs) (Expr a, as) = Result fs as
   fapply f (a, as) = (f',  Cast a : asList, i)
      where (f', asList, i) = fapply (Cast f :: Expr fs) (as :: as)

-- | Wraps result in all cases
wrapCall :: Apply fo ac => Expr fo -> ac -> Expr (Result fo ac)
wrapCall f a = AnonFunc Nothing args [ Return $ FuncCall f' (a' <> args) ]
   where (f', a', i) = fapply f a
         args = map (ex . intPref "_") [1..i]
         {- LATER TODO: have typed source of prefixes -}

doCall f a = FuncCall f' (a' <> args)
   where (f', a', i) = fapply f a
         args = map (ex . intPref "a") [1..i]

intPref p i = p <> TS.pack (show i)
