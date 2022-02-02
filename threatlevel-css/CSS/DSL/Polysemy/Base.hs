module CSS.DSL.Polysemy.Base where

import Common.Prelude
import Common.Polysemy hiding (run)
import qualified Data.Text as TS
import Data.DList as DList

import Polysemy qualified
import Polysemy.State
import Polysemy.Reader
import Polysemy.Writer

import CSS.Syntax
import CSS.DSL.Polysemy.Effect
import qualified Render

-- * @CSS@ to @Base@

type Names = Infinite TS.Text

type Base' rest = Append Base rest
type Base =
  [ Writer OuterRules
  , State Names
  , Reader Selector
  ]

getFreshClassBase :: Members Base r => Sem r Class
getFreshClassBase = do
  Infinite x xs <- get
  put xs
  return $ Class x

-- | Generate a class, bind declarations @m@ to it
cssBase :: Members Base r => m a -> Sem r Class
cssBase m = do
  c <- getFreshClassBase
--  emitForBase undefined (selFrom c) m
  undefined
--  return $ c

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
  -> Sem r a -> Sem b (OuterRules, a)
runDslBase f m = do
  s <- getSelectorBase @b
  fresh0 <- get @Names
  let (fresh1, (rs, a)) = f s fresh0 m :: BaseResult a
  put @Names fresh1
  return (rs, a)

cssToBase :: (Members Base r) => Sem (CSS : r) a -> Sem r a
cssToBase = interpretH $ \case
  Css m -> do
    class_ <- pureT =<< getFreshClassBase
    runTSimple m $> class_
    undefined
  GetFreshClass -> pureT =<< getFreshClassBase
  GetSelector -> pureT =<< getSelectorBase
  EmitFor s m -> pureT =<< emitForBase' s m
  EmitRules rs -> pureT =<< tell rs
  ExecDsl m -> let f = undefined in pureT =<< runDslBase f m

  where
    emitForBase' s m = let f = undefined in emitForBase f s m

type RunBase a = Selector -> Names -> a
type BaseResult a = (Names, (OuterRules, a))

-- | Run monomorphic @Base@
runBase :: RunBase (Sem (Base' r) a -> Sem r (BaseResult a))
runBase r s m = m
  & runWriter @Rules
  & runState s
  & runReader r

-- * @Prop@ to @Base@

propToBase :: (Members Base r) => Sem (Prop : r) a -> Sem r a
propToBase = interpret $ \case
  Prop property value -> do
    selector <- getSelectorBase
    let rule = Plain $ mkRule selector (pure $ Declaration property value)
    tell @OuterRules $ pure rule

type MonoCSS' rest = Sem (CSS : Prop : Append Base rest)
type MonoCSS = Sem (CSS : Prop : Base)

-- * Interpret @CSS@ to @Base@

run :: RunBase (MonoCSS' r a -> Sem r (BaseResult a))
run r s = cssToBase ^ propToBase ^ runBase r s

-- | Run monomorphic @CSS@ from empty start
runFresh :: MonoCSS' r a -> Sem r (BaseResult a)
runFresh m = run (selFrom Any) identifiers m

getRules :: forall a . RunBase (MonoCSS a -> OuterRules)
getRules r s m = rs
  where
    (_, (rs, _)) = Polysemy.run $ run r s m :: BaseResult a

getRulesFresh :: forall a . MonoCSS a -> OuterRules
getRulesFresh m = rs
  where
    (_, (rs, _)) = Polysemy.run $ runFresh m :: BaseResult a

instance Render.Render (MonoCSS a) where
  type Conf (MonoCSS a) = CSS.Syntax.Conf
  renderM = Render.renderM . getRulesFresh

-- * Compat

rulesFor :: SelectorFrom a => a -> MonoCSS a -> OuterRules
rulesFor slike m = getRules (selFrom slike) identifiers m

-- * Monomorphic plain @Prop@

propToWriter :: Member (Writer Declarations) r => Sem (Prop : r) a -> Sem r a
propToWriter = interpret $ \case
  Prop property value ->
    tell @Declarations $ pure $ Declaration property value

type MonoProp' r = Sem (Prop : Writer Declarations : r)
type MonoProp = MonoProp' '[]

runProp :: MonoProp' r a -> Sem r (Declarations, a)
runProp = propToWriter ^ runWriter

instance Render.Render (MonoProp a) where
  type Conf (MonoProp a) = CSS.Syntax.Conf
  renderM m = Render.renderM ds
    where (ds, _) = Polysemy.run $ runProp m
