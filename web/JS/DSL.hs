{-# LANGUAGE ExtendedDefaultRules #-}
module JS.DSL
  ( module JS.DSL
  , M, State(..), JS.Conf(..), runM, new, library, Function, Arguments, mkCode
  , HasConf(..)
  , HasRenderConf(..)

  -- * JS.Syntax
  , Statement(BareExpr)
  , Expr(Undefined, Null, Par, Lit, Cast, AnonFunc, Raw)
  , Attr(..)
  , Literal(..)
  , Code
  , call, call0, call1, (!.), (!-), (.!), (=:), ex
  ) where

import Prelude (Float, fromRational, toRational, Fractional, Rational)
import Prelude2 hiding ((.-), for, (.=), (.>), Empty)
import qualified Prelude2 as Pr
import qualified Data.Set as S
import qualified Data.Hashable as H
import Data.Default
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import JS.Syntax hiding (S, putStrLn, Conf)
import JS.DSL.Internal as JS
import Render
import JS.Render hiding ((=:))

new' :: TS.Text -> Expr a -> M r (Expr a)
new' n e = bool ignore name =<< asks (^.namedVars)
   where
      name = bind VarDef e . Name =<< pushNamedExpr n e
      ignore = new e

bare :: Expr a -> M r ()
bare e  = tell [ BareExpr e ]

block    = new    <=< blockExpr
block' n = new' n <=< blockExpr

-- * Control flow

ternary :: Expr Bool -> Expr a -> Expr a -> Expr a
ternary = Ternary

ifmelse :: Expr Bool -> M r a -> Maybe (M r a) -> M r ()
ifmelse cond true mFalse = do
   trueCode <- mkCode true
   mElseCode <- maybe (return Nothing) (fmap Just . mkCode) mFalse
   tell [IfElse cond trueCode mElseCode]

ifelse :: Expr Bool -> M r a -> M r a -> M r ()
ifelse c t e = ifmelse c t (Just e)

ifonly :: Expr Bool -> M r a -> M r ()
ifonly c t   = ifmelse c t Nothing

for :: Expr r -> M r a -> M r ()
for cond code = tell . (:[]) . f =<< mkCode code
   where f = For Empty cond Empty

forin expr f = do
   name <- next
   tell [ForIn name expr [BareExpr . call1 f $ EName name]]

while :: Expr r -> M r a -> M r ()
while cond code = tell . (:[]) . f =<< mkCode code
   where f = While cond

retrn :: Expr a -> M a ()
retrn e = tell $ [Return $ Cast e]

empty :: M a ()
empty = tell [Empty]

infixr 4 .=
(.=) :: Expr a -> Expr b -> M r ()
lhs .= rhs = tell [BareExpr $ lhs =: rhs]

type Promise = Expr

await :: Expr a -> JS.M r (Expr a)
await = new . JS.Syntax.Await

-- | Make a promise out of a function through async
promise :: Function f => f -> JS.M r (Promise b)
promise f = call0 <$> async f

browser :: M r Browser
browser = asks (view JS.browser)

blockExpr :: M r a -> M r (Expr r)
blockExpr = fmap (AnonFunc Nothing []) . mkCode
-- ^ Writes argument 'M r a' to writer and returns a callable name

arguments = ex "arguments"

-- * Typed functions

newf, async, generator :: Function a => a -> M r (Expr (Arguments a))
newf = new <=< func AnonFunc
async = new <=< func Async
generator = new <=< func Generator

newf' :: Function a => TS.Text -> a -> M r (Expr (Arguments a))
newf' n = new' n <=< func AnonFunc

-- | Create function, getting state and reader from enclosing monad.
func
  :: Function a
  => (Maybe Name -> [Expr ()] -> Code (Final a) -> Expr (Arguments a))
  -> a
  -> M parent (Expr (Arguments a))
func constr f = do
  (a, s) <- funcPrim constr <$> ask <*> get <*> pure f
  put s *> pure a

-- | Create function, starting from empty state and reader
funcPure :: Function a => a -> Expr (Arguments a)
funcPure = funcPrim AnonFunc def def <&> fst

f -/ (a :: Expr a) = wrapCall f (a, ())

f `a1` a = doCall f (a,())
f `a2` (a,b) = doCall f (a,(b,()))
f `a3` (a,b,c) = doCall f (a,(b,(c,())))
f `a4` (a,b,c,d) = doCall f (a,(b,(c,(d,()))))
f `a5` (a,b,c,d,e) = doCall f (a,(b,(c,(d,(e,())))))

a !/ b = call0 (a !. b)
a !// b = call1 (a !. b)

math name = ex "Math" !. name

-- ** Operators (untyped)

e1 .==  e2 = Op $ OpBinary   Eq e1 e2
e1 .=== e2 = Op $ OpBinary  EEq e1 e2
e1 .!=  e2 = Op $ OpBinary  NEq e1 e2
e1 .!== e2 = Op $ OpBinary NEEq e1 e2

infix 4 .==
infix 4 .===
infix 4 .!=
infix 4 .!==

e1 .&& e2 = Op $ OpBinary And e1 e2
e1 .|| e2 = Op $ OpBinary Or e1 e2

infixr 3 .&&
infixr 2 .||

e1 .<  e2  = Op $ OpBinary Lt e1 e2
e1 .>  e2  = Op $ OpBinary Gt e1 e2
e1 .<= e2 = Op $ OpBinary LEt e1 e2
e1 .>= e2 = Op $ OpBinary GEt e1 e2

infix 4 .<
infix 4 .>
infix 4 .<=
infix 4 .>=

e1 .+ e2 = Op $ OpBinary Plus e1 e2
e1 .- e2 = Op $ OpBinary Minus e1 e2
e1 .* e2 = Op $ OpBinary Mult e1 e2
e1 ./ e2 = Op $ OpBinary Div e1 e2
e1 .% e2 = Op $ OpBinary Modulus e1 e2
(%) = (.%)

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./
infixl 7 .%
infixl 7  %

-- * Literals

class    ToLiteral a       where lit :: a -> Expr b
instance ToLiteral (Expr a) where lit = Cast
instance ToLiteral Int     where lit = Lit . Integer . toInteger
instance ToLiteral Integer where lit = Lit . Integer
instance ToLiteral Rational where lit = Lit . Double . fromRational
instance ToLiteral Double  where lit = Lit . Double
instance ToLiteral Bool    where lit = Lit . Bool
instance ToLiteral TS.Text  where lit = Lit . String
instance ToLiteral TL.Text where lit = lit . TL.toStrict
instance ToLiteral String  where lit = lit . TL.pack

instance {-# OVERLAPPABLE #-} ToLiteral a => ToLiteral [a] where
   lit = Lit . Array . map lit

instance ToLiteral v => ToLiteral [(TS.Text, v)] where
   lit li = Lit $ Object $ map f li
      where f (k, v) = (Left $ Name k, lit v)

ck f = lit . map (first f)
instance ToLiteral v => ToLiteral [(TL.Text, v)] where
   lit = ck TL.toStrict
instance ToLiteral v => ToLiteral [(String, v)] where
  lit = ck TS.pack

instance IsString (Expr a) where
   fromString s = lit s

(!-) :: ToLiteral b => Expr a -> b -> Expr c -- TODO add types
(!-) a b = Arr a (lit b)

instance Num (Expr a) where
   fromInteger s = lit s
   (+) = (.+)
   (-) = (.-)
   (*) = (.*)
   negate n = 0 - n
   abs = call1 (math "abs")
   signum = call1 (math "sign")
instance Fractional (Expr a) where
   fromRational s = lit s
   (/) = (./)

a .+= b = a .= (a .+ b)
a .-= b = a .= (a .- b)
a .*= b = a .= (a .* b)
a ./= b = a .= (a ./ b)

pr :: M r a -> IO ()
pr = TL.putStrLn . render (Indent 2) . snd . fst . runM def def

-- * Modules

lib :: M r (Expr a) -> M r (Expr a)
lib mcode = let
    codeText = render Minify . snd . fst . runM def def $ mcode -- fix: take config from somewhere
    codeHash = H.hash codeText
    nameExpr = EName $ Name $ "h" <> TS.replace "-" "_" (TL.toStrict $ tshow codeHash)
  in do
  set <- gets (^.library)
  when (Pr.not $ codeHash `S.member` set) $ do
    f <- mcode
    nameExpr .= f
    modify (library %~ S.insert codeHash)
  return nameExpr

instance Render (M r a) where
  type Conf (M r a) = JS.Render.Conf
  renderM = renderM . snd . fst . runM def def
