{-# LANGUAGE ExtendedDefaultRules #-}
module JS_Monad
   (
   -- | JSM meta
     M, runM, eval, eval', pr, S(S), def

   -- | JSM primitives
   , new, new', newf, newf' --, {-newl,-} named, namedF
   , a1, a2
   , func, call, call0, call1, bare, arg
   , retrn
   , lit
   , ex
   , browser -- s
   , block


   -- | JS reexports
   , Code
   , Expr(Undefined, Null) -- , True, False)
   -- , Expr'
   , E(..) -- , ev
   , rawStm, rawExpr

   -- | Attribute and array indexing
   , (!.), (.!),  {- shorthand: -} (!-)

   -- | Operators
   , (.=)
   , (.==), (.!=), (.===), (.!==)
   , (.&&), (.||)
   , (.<), (.>), (.<=), (.>=)
   , (.+), (.-), (.*), (./)

   -- | Control flow
   , for, forin
   , ifelse, ifonly
   , ternary

   -- | Defined variables
   , arguments
   )
   where

import Prelude2 hiding ((.-), for, (.=), (.>))

import Text.Exts

import Data.Default

import qualified Data.IntMap as IM
import qualified Data.Set as S

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as TLIO
import Data.String

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

import JS_Syntax hiding (S)
import qualified JS_Syntax as JS
import qualified JS_Types  as JT
import JS_Ops_Untyped
import Common
import IdentifierSource

import Web_Client_Browser

-- * Construction

declareLenses [d|
   type W r = Code r
   type R = (Browser, Bool {- allow explicit variable names -})
   type Idents = [String]
   data S = S {
        counter :: Int
      , nameds :: S.Set Text
      , idents :: Idents
      }
   |]

instance Default S where def = S 0 S.empty jsIdentifierSource
defS = def :: S
instance Default R where def = (Unknown, True)

type M r = WriterT (W r) (StateT S (ReaderT R Identity))

runM :: R -> S -> M r a -> ((a, W r), S)
runM r s = id . rd . st . wr
   where id = runIdentity
         st = flip runStateT s
         wr = runWriterT
         rd = flip runReaderT r

run :: M r a -> ((a, W r), S)
run = runM def def
runDef = runM def def

eval :: M r a -> W r
eval    = snd . fst . run
eval' b = snd . fst . runM b def

-- evalN :: Int -> M r a -> W r
-- evalN    n = snd . fst . runM def (def & counter .~ n) -- (n, IM.empty)
fromNext b s = snd . fst . runM b   (s & idents %~ tail)

exec :: M r a -> a
exec = fst . fst . run

browser = asks fst

-- * Core

next :: M r Name
next = Name . T.pack . head <$> (idents <<%= tail)

new :: Expr a -> M r (Expr a)
new e = bind e =<< next

bind expr name = do
   define name $ Cast expr
   return $ Cast $ EName name
   where define name expr = tell [ VarDef name [] expr ]

pushNamedExpr :: Text -> Expr a -> M r Text
pushNamedExpr n e = do
   modify (nameds %~ S.insert n)
   return n

new' :: Text -> Expr a -> M r (Expr a)
new' n e = bool ignore name =<< asks snd
   where
      name = bind e . Name =<< pushNamedExpr n e
      ignore = new e

{- | Evaluate JSM code to Code aka W r aka [Statement]
     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: M sub a -> M parent (W sub)
mkCode mcode = fromNext <$> ask <*> get <*> pure mcode

bare :: Expr a -> M r ()
bare e  = tell [ BareExpr e ]

block    = new    <=< blockExpr
block' n = new' n <=< blockExpr

-- * Control structures

ternary :: Expr JT.Bool -> Expr a -> Expr a -> Expr a
ternary = Ternary

-- ifmelse :: Expr JT.Bool -> M r a -> M r (a) -> M r a
ifmelse cond true mFalse = do
   trueCode <- mkCode true
   mElseCode <- maybe (return Nothing) (fmap Just . mkCode) mFalse
   tell [ IfElse cond trueCode mElseCode ]

-- ifelse :: Expr JT.Bool -> M r a -> M r a -> M r a
ifelse c t e = ifmelse c t (Just e)
ifonly c t   = ifmelse c t Nothing

retrn :: Expr a -> M a ()
retrn e = tell $ [ Return $ Cast e ]

untype = Cast :: Expr a -> Expr ()

pr :: M r a -> IO ()
pr = TLIO.putStrLn . ev . eval

ex txt = EName $ Name txt

(!.) :: Expr a -> Name -> Expr b
(!.) expr attr = EAttr $ Attr (Cast expr) attr


(.!) expr key  = Arr expr key

(!-) :: ToULiteral b => Expr a -> b -> Expr c -- TODO add types
(!-) a b = Arr a (ulit b)

infixr 8 .=
(.=) :: Expr a -> Expr b -> M r ()
lhs .= rhs = tell [ Def lhs rhs ]

a .+= b = a .= (a .+ b)
a .-= b = a .= (a .- b)
a .*= b = a .= (a .* b)
a ./= b = a .= (a ./ b)

-- ** Functions

-- *** Untyped

blockExpr :: M r a -> M r (Expr r)
blockExpr = fmap (FuncDef []) . mkCode
-- ^ Writes argument 'M r a' to writer and returns a callable name

call :: Expr a -> [Expr b] -> Expr c
call  f as = FuncCall f as

call0 f = FuncCall f []
call1 f a = FuncCall f [a]
arg n = Arr "arguments" (lit n)

-- *** Typed
tcall :: (Show a, Args a) => Expr (a, r) -> a -> (Expr r)
tcall f as = TypedFCall f as

for :: Expr r -> M r a -> M r ()
for cond code = tell . (:[]) . f =<< mkCode code
   where f = For (rawStm "") cond (rawStm "")

forin expr f = do
   name <- next -- (+1) <$> gets (^.counter)
   tell [ ForIn name expr [ BareExpr . call1 f $ EName name ] ]

rawStm = BareExpr . rawExpr
rawExpr = Raw

-- * Predefined names

arguments = ex "arguments"

-- * Typing machinery for functions

newf     = new    <=< func
newf' n  = new' n <=< func

-- | Create function, getting state and reader from enclosing monad.
func :: Function a => a -> M parent (Expr (Arguments a))
func f = funcPrim <$> ask <*> get <*> pure f

-- | Create function, starting from empty state and reader
funcPure :: Function a => a -> Expr (Arguments a)
funcPure = funcPrim def def

-- | Create function from a literal: provide JSM state and reader
funcPrim :: Function a => R -> S -> a -> Expr (Arguments a)
funcPrim r s fexp = FuncDef args code
   where (type_, args, code) = fst . fst $ runM r s $ funcLit fexp

-- | The 'Function' class turns function literals into typed
-- functions.
class Function a where
   type Arguments a
   type Final a
   funcLit :: a -> M self (Arguments a, [Expr ()], Code (Final a))
instance Function (M r a) where
   type Arguments (M r a) = Proxy r
   type Final (M r a) = r
   funcLit f = (Proxy, [], ) <$> mkCode f
instance (Function b) => Function (Expr a -> b) where
   type Arguments (Expr a -> b) = (Expr a, Arguments b)
   type Final     (Expr a -> b) = Final b
   funcLit f = do
      x <- EName <$> next
      (a, a', b) <- funcLit (f x)
      return ((x,a), Cast x : a', b)

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
wrapCall f a = FuncDef args [ Return $ FuncCall f' (a' <> args) ]
   where (f', a', i) = fapply f a
         args = map (ex . intPref "_") [1..i]
         {- LATER TODO: have typed source of prefixes -}

doCall f a = FuncCall f' (a' <> args)
   where (f', a', i) = fapply f a
         args = map (ex . intPref "a") [1..i]

intPref p i = p <> tshow i

f -/ (a :: Expr a) = wrapCall f (a, ())

{- ** THE DREAM ** -}
test = let
   in do
   return1 :: Expr (Expr JT.String, Proxy JT.NumberI)
      <- newf' "ret1" $ \ (s :: Expr JT.String) ->
         retrn (lit (1::Int) :: Expr JT.NumberI)
   addArgs <- newf' "addArgs" $ \ (a :: Expr JT.NumberI) (b :: Expr JT.NumberI) -> do
      retrn $ a .+ b

   bare $ addArgs `a2` (lit (2::Int), lit (3::Int))
   -- bare $ addArgs -/ lit ("string" :: String) -/ lit (3::Int)     -- errors due to wrong type of argument
   -- bare $ addArgs -/ lit (2::Int) -/ lit (3::Int) -/ lit (4::Int) -- errors due to type mismatch (caused by too many arguments)

f `a1` a = doCall f (a,())
f `a2` (a,b) = doCall f (a,(b,()))
f `a3` (a,b,c) = doCall f (a,(b,(c,())))
f `a4` (a,b,c,d) = doCall f (a,(b,(c,(d,()))))
f `a5` (a,b,c,d,e) = doCall f (a,(b,(c,(d,(e,())))))

data FA a b = FA (Expr a, Proxy b)
instance Category FA where
   id = u
   f . g = u
