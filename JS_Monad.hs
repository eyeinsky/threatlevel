module JS_Monad where

import Prelude2 hiding ((.-))
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

import Web_Client_Browser

import Debug.Trace

--
-- Construction
--

type W r = Code r
type R = (Browser, Bool {- allow explicit names -})
data S = S {
     counter :: Int
   , nameds :: S.Set Text
   }


instance Default S where def = S 0 S.empty
defS = def :: S
instance Default R where def = (Unknown, True)

type M r = WriterT (W r) (StateT S (ReaderT R Identity))


runM :: R -> S -> M r a -> ((a, W r), S)
runM b s = id . rd . st . wr
   where id = runIdentity
         st = flip runStateT s
         wr = runWriterT
         rd = flip runReaderT b

run :: M r a -> ((a, W r), S)
run = runM def def 
runDef = runM def def 

eval :: M r a -> W r
eval    = snd . fst . run
eval' b = snd . fst . runM b def

evalN :: Int -> M r a -> W r
evalN    n = snd . fst . runM def (def { counter = n }) -- (n, IM.empty)
evalN' b n = snd . fst . runM b   (def { counter = n })

exec :: M r a -> a
exec = fst . fst . run

browser = asks fst

--
-- Core
--

nextIncIdent :: M r Int
nextIncIdent = do
   s <- get
   let i = counter s
   put (s { counter = i+1})
   return i

pushExpr :: Expr a -> M r Int
pushExpr _ = nextIncIdent
pushNamedExpr :: Text -> Expr a -> M r Text
pushNamedExpr n e = do
   modify $ \s -> s { nameds = S.insert n (nameds s) }
   return n

define name expr = tell [ VarDef name [] expr ]

int2text = intPref "v"
intPref p i = p <> tshow i

newMaker f e = do
   name <- Name . either int2text id <$> f e
   define name $ Cast e
   return $ Cast $ EName name

new :: Expr a -> M r (Expr a)
new e = newMaker (fmap Left . pushExpr) e

new' :: Text -> Expr a -> M r (Expr a)
new' n e = bool ignore name =<< asks snd
   where
      name = newMaker (fmap Right . pushNamedExpr n) e
      ignore = new e

{- | Evaluate JSM code to Code aka W r aka [Statement]
     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: M sub a -> M parent (W sub)
mkCode mcode = evalN' <$> ask <*> nextIdent <*> pure mcode
   where nextIdent = (+1) <$> gets counter

bare :: Expr a -> M r ()
bare e  = tell [ BareExpr e ]

block    = new    <=< blockExpr 
block' n = new' n <=< blockExpr


--
-- Control structure 
--

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

--
--
--

pr :: M r a -> IO ()
pr = TLIO.putStrLn . ev . eval

ex txt = EName $ Name txt

(!.) :: Expr a -> Name -> Expr b
(!.) expr attr = EAttr $ Attr (Cast expr) attr


(.!) expr key  = Arr expr key

(!-) :: ToULiteral b => Expr a -> b -> Expr c -- TODO add types
(!-) a b = Arr a (ulit b)

infixr 8 .=
lhs .= rhs = tell [ Def lhs rhs ]

a .+= b = a .= (a .+ b)
a .-= b = a .= (a .- b)
a .*= b = a .= (a .* b)
a ./= b = a .= (a ./ b)

-- ** Functions

-- *** Untyped
blockExpr :: M r a -> M r (Expr r)
blockExpr = fmap (FuncDef []) . mkCode -- writes M a to writer, returns name
call :: Expr a -> [Expr b] -> Expr c
call f as = FuncCall f as
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
   i <- (+1) <$> gets counter  
   tell [ ForIn (Name $ int2text i) expr [ BareExpr . call1 f $ ex $ int2text i ] ]

rawStm = BareExpr . rawExpr
rawExpr = Raw


-- * Predefined names

arguments = ex "arguments"


-- * The typing machinery for functions

newf     = new    <=< func
newf' n  = new' n <=< func


-- | Returns a function definition Expr
func :: Function a => a -> M parent (Expr (Arguments a))
func f = do
   (argType,b,c) <- funcLit f
   return $ Cast $ FuncDef b c -- `asTypeOf` 

class Function a where
   type Arguments a
   type Final a
   funcLit :: a -> M self (Arguments a, [Expr ()], Code (Final a))
instance Function (M r a) where
   type Arguments (M r a) = JT.Proxy r
   type Final (M r a) = r
   funcLit f = (JT.Proxy, [], ) <$> mkCode f
instance (Function b) => Function (Expr a -> b) where
   type Arguments (Expr a -> b) = (Expr a, Arguments b)
   type Final     (Expr a -> b) = Final b
   funcLit f = do
      x <- ex . int2text <$> nextIncIdent
      (a, a', b) <- funcLit (f x)
      return ((x,a), Cast x : a', b)

class Apply f a where
   type Result f a
   fapply :: Expr f -> a -> (Expr (Result f a), [Expr ()], Int)
instance Apply (JT.Proxy f) () where
   {- Function is exhausted, start returning. -}
   type Result (JT.Proxy f) () = JT.Proxy f
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
appExpr :: Apply fo ac => Expr fo -> ac -> Expr (Result fo ac)
appExpr f a = FuncDef args [ Return $ FuncCall f' (a' <> args) ]
   where (f', a', i) = fapply f a
         args = map (ex . intPref "a") [1..i]
         {- LATER TODO: argument integers are prefixed with "a".
            Though this doesn't interfere with my "v" prefixes
            generated from the 'M r a', then this could be a
            possible source of unsafety if I should ever change
            the identifier generation machinery.
          -}  

f -/ (a :: Expr a) = appExpr f (a, ())

{- ** THE DREAM **
-}
test = let 
   in do
   return1 <- newf' "ret1" $ \ (s :: Expr JT.String) ->
      retrn (lit (1::Int) :: Expr JT.NumberI)
   addArgs <- newf' "addArgs" $ \ (a :: Expr JT.NumberI) (b :: Expr JT.NumberI) -> do
      retrn $ a .+ b
   -- retrn $ appExpr addArgs ({-lit (2::Int),-} (lit (2::Int), ()))
   retrn $ addArgs -/ lit (2::Int) -/ lit (3::Int) -- , (lit (2::Int), ()))
