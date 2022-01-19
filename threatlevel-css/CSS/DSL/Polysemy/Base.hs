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

prop :: Members Base r => TS.Text -> Value -> Sem r ()
prop k v = emitDeclarationsBase $ pure $ Declaration k v

-- * Interpret @CSS@ to @Base@

cssToBase :: Members Base r => Sem (CSS : r) a -> Sem r a
cssToBase = interpret $ \case
  GetFreshClass -> Class <$> getFreshClassBase
  EmitRules s ds -> emitRulesBase $ pure $ mkRule s ds
  EmitDeclarations ds -> emitDeclarationsBase ds

-- * Monomorphic @Base@

type BaseResult a = (Names, (Declarations, (Rules, a)))

-- | Run monomorphic @Base@
runBase :: Selector -> Names -> Sem Base a -> BaseResult a
runBase r s m = m
  & runWriter @Rules
  & runWriter @Declarations
  & runState s
  & runReader r
  & Polysemy.run

-- | Run monomorphic @CSS@
run :: Selector -> Names -> Sem (CSS : Base) a -> BaseResult a
run r s m = m
  & cssToBase
  & runBase r s

-- | Run monomorphic @CSS@ from empty start
runEmpty :: Sem (CSS : Base) a -> BaseResult a
runEmpty m = run (selFrom $ Tag "") identifiers m

instance Render (Sem (CSS : Base) a) where
  renderM m = undefined
