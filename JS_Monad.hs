{-# LANGUAGE ExtendedDefaultRules #-}
module JS_Monad
   (
   -- | JSM meta
     M, runM, eval, eval', pr, S(S), def
   
   -- | JSM primitives
   , new, newf, newf', newl, named, namedF
   , func, call, call0, call1, bare, arg
   , retrn
   , lit
   , ex
   , browsers
   , Var'(..)


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
   , domElem
   , tagById, tagById'
   , on
   , findBy

   -- | DOM objects -> Event
   , KeyboardEvent(..), onload

   -- | Zepto
   , zepto, zById, mkZ
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

pr :: M a -> IO ()
pr = TLIO.putStrLn . ev . eval

browsers f = ask >>= f

type Var = Expr'

data Var' a where
   Var' :: Expr a -> Var' a

v2v = int2name
int2name i = Name $ T.pack $ 'v' : show i
int2var = EName . int2name

pushExpr :: Expr' -> M Int
pushExpr e = do
   s <- get
   let i = counter s -- m = u -- IM.insert i e m)
   put (s { counter = i+1})
   return i

pushNamedExpr :: Text -> Expr' -> M Name
pushNamedExpr n e = do
   modify $ \s -> s { nameds = S.insert n (nameds s) }
   return $ Name n

define name expr = tell [ VarDef name [] expr ]

new :: Expr' -> M Expr'
new e = do
   name <- v2v <$> pushExpr e
   define name e
   return $ EName name


{- | Evaluate JSM code to Code aka W aka [Statement]
     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: M a -> M W
mkCode m = evalN' <$> ask <*> next <*> pure m
   where next = (+1) <$> gets counter

--
-- 
--

bare e = tell [ BareExpr e ]; bare :: Expr' -> M ()
newf m = new =<< func m 
newf' n m = do
   var <- new =<< func m
   ex n .= var
   return var

newl l = new $ lit l

named :: Text -> Expr' -> M Expr'
named n e = do
   name <- pushNamedExpr n e
   define name e
   return $ EName name
namedF n m = named n =<<$ func m

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

ex str = EName $ Name str

(!.) :: Expr' -> Name -> Expr'
(!.) expr attr = EAttr $ Attr expr attr
(.!) expr key  = Arr expr key

(!-) a b = Arr a (lit b)


infixr 8 .=
lhs .= rhs = tell [ Def lhs rhs ]


-- ** Functions

-- | Takes func body, writes Code to main writer,
--   returns Var, where the func got bound.
func :: M a -> M Expr'
func = fmap FuncExpr . mkCode

arg n = Arr "arguments" (lit n)

call :: Expr a -> [Expr a] -> Expr a
call f as = FuncCall f as

call0 f = FuncCall f []
call1 f a = FuncCall f [a]

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
   tell [ ForIn (int2name i) expr [ BareExpr . call1 f $ int2var i ] ]

rawStm = BareExpr . rawExpr
rawExpr = Raw


-- * Taken but renameable JS names

arguments = ex "arguments"

-- * DOM

window = ex "window"
document = ex "document"
location = window !. "location"
domElem x = call1 (document !. "getElementById") $ lit x


-- onloadIs :: Code -> Statement
onloadIs code = onload .= FuncExpr code

onload = ex "window" !. "onload"

tagById str = FuncCall "$" [ lit $ "#" <> str] 
zById = tagById 
tagById' expr =  FuncCall "$" [ expr ]
mkZ expr = FuncCall "$" [ expr ]
zepto expr = FuncCall "$" [ expr ]

mkTag str = FuncCall "$" [ lit $ sur "<" ">" str ]

on el ev jsm = do
   code <- mkCode jsm 
   bare $ call (el !. "on") [ f ev, FuncExpr code ]
   where
      f = lit . T.toLower . tshow 



-- ** DOM API

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



