module CSS.DSL.Polysemy.Base where

import Common.Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.DList as DList

import Polysemy hiding (run)
import qualified Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Writer

import CSS.Syntax
import CSS.DSL.Polysemy.Effect
import qualified Render

-- * Polymorphic @Base@

type Names = Infinite TS.Text

type Base =
  [ Writer Rules
  , Writer Declarations
  , State Names
  , Reader Selector
  ]

getFreshClassBase :: Members Base r => Sem r TS.Text
getFreshClassBase = do
  Infinite x xs <- get
  put xs
  return x

emitRulesBase :: Members Base r => Rules -> Sem r ()
emitRulesBase rules = tell rules

emitDeclarationsBase :: Members Base r => Declarations -> Sem r ()
emitDeclarationsBase ds = tell ds

-- | Run @sub@ to final result with @f@. @f@ is passed in from the
-- enclosing monad.
withDerivedSelectorBase
  :: forall r b a
   . Members Base b
  => (forall _a . RunBase (Sem r _a -> BaseResult _a))
  -> (Selector -> Selector) -> Sem r a -> Sem b ()
withDerivedSelectorBase f mod sub = do
  selector <- ask @Selector
  fresh0 <- get @Names
  let (fresh1, (ds, (rs, a))) = f selector fresh0 sub :: BaseResult a
  put @Names fresh1
  return ()

prop :: Members Base r => TS.Text -> Value -> Sem r ()
prop k v = emitDeclarationsBase $ pure $ Declaration k v

-- * Interpret @CSS@ to @Base@

cssToBase
  :: forall r b a
   . (Members Base b, r ~ (CSS b : b))
  => (forall _a . RunBase (Sem r _a -> BaseResult _a))
  -> Sem (CSS b : b) a -> Sem b a
cssToBase f = interpret $ \case
  GetFreshClass -> Class <$> getFreshClassBase
  EmitRules s ds -> emitRulesBase $ pure $ mkRule s ds
  EmitDeclarations ds -> emitDeclarationsBase ds
  WithDerivedSelector mod (sub :: Sem r _a) ->
    withDerivedSelectorBase f mod sub

-- * Monomorphic @Base@

type RunBase a = Selector -> Names -> a
type BaseResult a = (Names, (Declarations, (Rules, a)))

-- | Run monomorphic @Base@
runBase :: RunBase (Sem Base a -> BaseResult a)
runBase r s m = m
  & runWriter @Rules
  & runWriter @Declarations
  & runState s
  & runReader r
  & Polysemy.run

-- | Run monomorphic @CSS@
run :: Selector -> Names -> Sem (CSS Base : Base) a -> BaseResult a
run r s m = m
  & cssToBase run
  & runBase r s

-- | Run monomorphic @CSS@ from empty start
runEmpty :: Sem (CSS Base : Base) a -> BaseResult a
runEmpty m = run (selFrom $ Tag "") identifiers m

instance Render (Sem (CSS Base : Base) a) where
  renderM m = undefined
