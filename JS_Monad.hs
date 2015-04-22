{-# LANGUAGE ExtendedDefaultRules #-}
module JS_Monad
   (
   -- | JSM meta
     M, runM, eval, eval', run, pr, S(S), def
   
   -- | JSM primitives
   , new, new'
   , block, block', blockExpr
   , newf , newf' , func

   , call, call0, call1, bare, arg

   , FL(..)

   , retrn
   , lit
   , ex
   , browsers
   , cast


   -- | JS reexports
   , Code, Code'
   , Expr(Undefined, Null, True, False)
   , Expr'
   , E(..) -- , ev
   , rawStm, rawExpr

   -- | Attribute and array index
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

   -- | DOM objects
   , window, location
   , on, findBy

   -- | DOM objects -> Event
   , KeyboardEvent(..), onload

   -- | Zepto
   , zepto
   )
   where

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

import Web_Client_Browser
import qualified Web_CSS as CSS
import Web_HTML

import Debug.Trace

--
-- Construction
--

type Text = JS.S

type W = Code ()
type R = Browser
data S = S {
     counter :: Int
   , nameds :: S.Set Text
   }
instance Default S where def = S 0 S.empty
defS = def :: S
instance Default R where def = Unknown

type M = WriterT W (StateT S (ReaderT R Identity))


runM :: R -> S -> M a -> ((a, W), S)
runM b s = id . rd . st . wr
   where id = runIdentity
         st = flip runStateT s
         wr = runWriterT
         rd = flip runReaderT b

run :: M a -> ((a, W), S)
run = runM def def 
runDef = runM def def 

eval :: M a -> W
eval    = snd . fst . run
eval' b = snd . fst . runM b def

-- | Start from n with var numbers
evalN :: Int -> M a -> W
evalN    n = snd . fst . runM def (def { counter = n }) -- (n, IM.empty)
evalN' b n = snd . fst . runM b   (def { counter = n })

exec :: M a -> a
exec = fst . fst . run

browser = ask

--
-- Core
--

nextIncIdent :: M Int
nextIncIdent = do
   s <- get
   let i = counter s
   put (s { counter = i+1})
   return i


pushExpr :: Expr a -> M Int
pushExpr _ = nextIncIdent -- m = u -- IM.insert i e m)

pushNamedExpr :: Text -> Expr a -> M Text
pushNamedExpr n e = do
   modify $ \s -> s { nameds = S.insert n (nameds s) }
   return n

define name expr = tell [ VarDef name [] expr ]

int2text = ("v"<>) . tshow

newMaker f e = do
   name <- Name . either int2text id <$> f e
   define name $ cast e
   return $ EName name

new :: Expr a -> M (Expr a)
new e = newMaker (fmap Left . pushExpr) e

new' :: Text -> Expr a -> M (Expr a)
new' n e = newMaker (fmap Right . pushNamedExpr n) e

{- | Evaluate JSM code to Code aka W aka [Statement]
     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: M a -> M W
mkCode mcode = evalN' <$> browser <*> nextIdent <*> pure mcode
   where nextIdent = (+1) <$> gets counter

bare e  = tell [ BareExpr e ]; bare :: Expr' -> M ()
block    = new    <=< blockExpr 
block' n = new' n <=< blockExpr


--
-- Control structure 
--

ternary :: Expr JT.Bool -> Expr' -> Expr' -> Expr'
ternary = Ternary

ifmelse cond true mFalse = do
   trueCode <- mkCode true
   mElseCode <- maybe (return Nothing) (fmap Just . mkCode) mFalse
   tell [ IfElse cond trueCode mElseCode ]

ifelse :: Expr' -> M a -> M b -> M ()
ifelse c t e = ifmelse c t (Just e)
ifonly c t   = ifmelse c t Nothing

retrn :: Expr' -> M ()
retrn e = tell $ [ Return e ]


--
--
--

pr :: M a -> IO ()
pr = TLIO.putStrLn . ev . eval

browsers f = ask >>= f

ex str = EName $ Name str

(!.) :: Expr' -> Name -> Expr'
(!.) expr attr = EAttr $ Attr expr attr
(.!) expr key  = Arr expr key

(!-) a b = Arr a (lit b)

infixr 8 .=
lhs .= rhs = tell [ Def lhs rhs ]


-- ** Functions

-- *** Untyped
blockExpr :: M a -> M Expr'
blockExpr = fmap FuncExpr . mkCode -- writes M a to writer, returns name
call :: Expr a -> [Expr a] -> Expr a
call f as = FuncCall f as
call0 f = FuncCall f []
call1 f a = FuncCall f [a]
arg n = Arr "arguments" (lit n)

-- *** Typed
tcall :: (Show a, Args a) => Expr (a, r) -> a -> (Expr r)
tcall f as = TypedFC f as



-- ** Operators

e1 .== e2 = Op $ OpBinary  Eq e1 e2
e1 .=== e2 = Op $ OpBinary  Eq e1 e2
e1 .!= e2 = Op $ OpBinary NEq e1 e2
e1 .!== e2 = Op $ OpBinary NEq e1 e2

e1 .&& e2 = Op $ OpBinary And e1 e2
e1 .|| e2 = Op $ OpBinary Or e1 e2

e1 .< e2  = Op $ OpBinary Lt e1 e2
e1 .> e2  = Op $ OpBinary Gt e1 e2
e1 .<= e2 = Op $ OpBinary LEt e1 e2
e1 .>= e2 = Op $ OpBinary GEt e1 e2

e1 .+ e2 = Op $ OpBinary BPlus  e1 e2
e1 .- e2 = Op $ OpBinary BMinus e1 e2
e1 .* e2 = Op $ OpBinary Mult e1 e2
e1 ./ e2 = Op $ OpBinary Div e1 e2

for :: Expr' -> M a -> M ()
for cond code = tell . (:[]) . f =<< mkCode code
   where f = For (rawStm "") cond (rawStm "")

forin expr f = do
   i <- (+1) <$> gets counter  
   tell [ ForIn (Name $ int2text i) expr [ BareExpr . call1 f $ ex $ int2text i ] ]

rawStm = BareExpr . rawExpr
rawExpr = Raw


-- * Predefined names

arguments = ex "arguments"


--
-- * DOM
--

window = ex "window"
document = ex "document"
location = window !. "location"

onloadIs code = onload .= FuncExpr code -- :: Code' -> Statement ()

onload = ex "window" !. "onload"

on el eventType fexpr = do
   bare $ call (el !. "on") [ str, fexpr ]
   where str = lit . T.toLower . tshow $ eventType


class FindBy a where
   findBy :: a -> JS.Expr' -- JS.Expr Element
instance FindBy CSS.Id where
   findBy (CSS.Id t) = docCall "getElementById" t
instance FindBy CSS.Class where
   findBy (CSS.Class a) = docCall "getElementsByClassName" a
instance FindBy CSS.TagName where
   findBy (CSS.TagName a) = docCall "getElementsByTagName" a

docCall f a = call1 (document !. f) (lit a)



createElement :: TagName -> JS.Expr'
createElement tn = docCall "createElement" $ unTagName tn

-- creates the expr to create the tree, returns top
treeCreateExpr :: HTML -> JS.Expr'
treeCreateExpr tr = FuncExpr . eval $ case tr of 
   TagNode tn mid cls ns -> do
      t <- new $ createElement tn
      maybe (return ()) (\id -> t !. "id" .= lit (unId id)) mid
      when (not . null $ cls) $ 
         t !. "className" .= lit (TL.unwords $ map unClass cls) 
      retrn t
   TextNode txt -> retrn $ docCall "createTextNode" txt


zepto expr = FuncCall "$" [ expr ]


{-
-- / mock 
type A1 = JT.Bool
type A2 = JT.String
type A3 = JT.Number
type R1 = JT.Object
type AS = (Expr A1, (Expr A2, (Expr A3, END)))
f :: Expr (AS, R1); f = u
as :: AS; as = u
t1 = tcall f as 
t2 = tcall (EName (Name "x")  :: Expr ((Expr Bool, ())  , JT.Number ))
           (EName (Name "a1") ::        Expr Bool, ())
-- mock / --}


func f = do
   (args', fexp) <- fl (args, f)
   new fexp
   where args = [] :: [Expr']
newf    = new    <=< func
newf' n = new' n <=< func

-- class to generate Code for function
class FL a where
   fl :: a -> M ([Expr'], Expr')
instance FL ([Expr'], M a) where
   fl (a, f) = let r = reverse a in do
      fexp <- FuncExprA r <$> mkCode f
      return $ (r, fexp)
instance FL ([Expr'], b) => FL ([Expr'], Expr' -> b) where
   fl (a, f) = do
      x <- ex . int2text <$> nextIncIdent
      fl (x:a, f x)
