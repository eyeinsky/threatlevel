{-# LANGUAGE ExtendedDefaultRules #-}
module JS.DSL
  ( module JS.DSL
  , M, State(..), JS.Conf(..), runM, new, library, Function, Arguments, mkCode
  , HasConf(..)
  , HasRenderConf(..)
  ) where

import Prelude2 hiding ((.-), for, (.=), (.>), Empty)
import Data.Default
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Writer
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import JS.Syntax hiding (S, putStrLn, Conf)
import JS.DSL.Internal as JS

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
