
module CSS.DSL.Polysemy.Base where

import Common.Prelude
import qualified Data.Text as TS
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
  , State Names
  , Reader Selector
  ]

getFreshClassBase :: Members Base r => Sem r TS.Text
getFreshClassBase = do
  Infinite x xs <- get
  put xs
  return x

getSelectorBase :: Members Base r => Sem r Selector
getSelectorBase = ask @Selector

-- | Run @sub@ to final result with @f@. @f@ is passed in from the
-- enclosing monad.
emitForBase
  :: forall r b a
   . Members Base b
  => (forall _a . RunBase (Sem r _a -> BaseResult _a))
  -> Selector -> Sem r a -> Sem b a
emitForBase f s m = do
  fresh0 <- get @Names
  let (fresh1, (rs, a)) = f s fresh0 m :: BaseResult a
  put @Names fresh1
  tell rs
  return a

runDslBase
  :: forall r b a
   . Members Base b
  => (forall _a . RunBase (Sem r _a -> BaseResult _a))
  -> Sem r a -> Sem b (Rules, a)
runDslBase f m = do
  s <- getSelectorBase @b
  fresh0 <- get @Names
  let (fresh1, (rs, a)) = f s fresh0 m :: BaseResult a
  put @Names fresh1
  return (rs, a)

-- * interpret @CSS@ to @Base@

cssToBase
  :: forall r b a
   . (Members Base b, r ~ (CSS b : b))
  => (forall _a . RunBase (Sem r _a -> BaseResult _a))
  -> Sem (CSS b : b) a -> Sem b a
cssToBase f = interpret $ \case
  GetFreshClass -> Class <$> getFreshClassBase
  GetSelector -> getSelectorBase
  EmitFor s m -> emitForBase f s m
  EmitRules rs -> tell rs
  ExecDsl m -> runDslBase f m

-- * Monomorphic @Base@

type MonoCSS = Sem (CSS Base : Base)
type RunBase a = Selector -> Names -> a
type BaseResult a = (Names, (Rules, a))

-- | Run monomorphic @Base@
runBase :: RunBase (Sem Base a -> BaseResult a)
runBase r s m = m
  & runWriter @Rules
  & runState s
  & runReader r
  & Polysemy.run

-- | Run monomorphic @CSS@
run :: RunBase (MonoCSS a -> BaseResult a)
run r s m = m
  & cssToBase run
  & runBase r s

-- | Run monomorphic @CSS@ from empty start
runFresh :: MonoCSS a -> BaseResult a
runFresh m = run (selFrom Any) identifiers m

instance Render.Render (MonoCSS a) where
  type Conf (MonoCSS a) = CSS.Syntax.Conf
  renderM m = Render.renderM rs
    where (_, (rs, _)) = runFresh m :: BaseResult a

-- * Compat

rulesFor :: SelectorFrom a => a -> MonoCSS a -> [OuterRule]
rulesFor slike m = rs
  where (_, (rs, _)) = run (selFrom slike) identifiers m

-- * Monomorphic with @Base' = Prop : Base@

type Base' = Prop : Base
type MonoCSS' = Sem (CSS Base' : Base')

propToBase
  :: forall r a . (Members Base r)
  => Sem (Prop : r) a -> Sem r a
propToBase = interpret $ \case
  Prop property value -> do
    selector <- getSelectorBase @r
    tell @Rules $ pure $ mkRule selector (pure $ Declaration property value)

run' :: RunBase (MonoCSS' a -> BaseResult a)
run' r s m = m
  & cssToBase run'
  & propToBase
  & runBase r s

instance Render.Render (MonoCSS' a) where
  type Conf (MonoCSS' a) = CSS.Syntax.Conf
  renderM m = Render.renderM rs
    where (_, (rs, _)) = run' (selFrom Any) identifiers m :: BaseResult a

-- * Monomorphic plain @Prop@

propToWriter :: Member (Writer Declarations) r => Sem (Prop : r) a -> Sem r a
propToWriter = interpret $ \case
  Prop property value ->
    tell @Declarations $ pure $ Declaration property value

type MonoProp = Sem [Prop, Writer Declarations]

runProp :: MonoProp a -> (Declarations, a)
runProp ds = ds
  & propToWriter
  & runWriter
  & Polysemy.run

instance Render.Render (MonoProp a) where
  type Conf (MonoProp a) = CSS.Syntax.Conf
  renderM m = Render.renderM ds
    where (ds, _) = runProp m
