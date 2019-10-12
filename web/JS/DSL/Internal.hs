{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module JS.DSL.Internal
  ( module Web.Browser
  , module JS.DSL.Internal
  ) where

import X.Prelude hiding ((.>), Empty, Const, State)

import Data.Default

import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Hashable as H

import qualified Data.Text.Lazy as TL
import qualified Data.Text as TS
import qualified Data.Text.IO as TLIO
import Data.String

import qualified JS.Syntax (Conf(..))
import JS.Syntax hiding (Conf)

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

instance {-# OVERLAPS #-} (HasConf c Conf) => HasBrowser c Browser where
  browser = conf.browser


instance Default State where
  def = State identifiers S.empty S.empty

instance Default Conf where
  def = Conf Unknown True (JS.Syntax.Indent 2)

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

-- *** Typed
tcall :: (Show a, Args a) => Expr (a, r) -> a -> (Expr r)
tcall f as = TypedFCall f as

-- * Typed functions

-- | Create function from a literal: provide JSM state and reader
funcPrim
  :: Function a
  => (Maybe Name -> [Name] -> Code (Final a) -> Expr (Type a))
  -> Conf -> State -> a -> (Expr (Type a), State)
funcPrim constr r s0 fexp = (constr Nothing args code, s1)
   where
     ((args, code), s1) = runM r s0 (funcLit fexp)

-- | The 'Function' class turns function literals into typed
-- functions.
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
   funcLit e = tell [Return $ Cast e] $> []
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
