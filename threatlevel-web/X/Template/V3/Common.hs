module X.Template.V3.Common
  ( module X.Template.V3.Common
  , module X.Template.Common
  ) where

import X.Prelude
import X
import X.Template.Common
import JS.DSL.MTL.Function


type Creates a = Expr a -> Expr (Context a)
type Updates a = Expr a -> Expr (Context a) -> Expr ()
type Gets a = Expr [Node] -> Expr a
type Slots = [Class]

-- * Slots

nSlots :: forall m. MonadWeb m => Int -> m Slots
nSlots n = replicateM n (css $ pure ())

-- | Create classes and all field getters
nSlotGetters :: MonadWeb m => Int -> m (Slots, [Gets Tag])
nSlotGetters n = do
  fields <- nSlots n
  getters <- mapM mkGet fields
  return (fields, getters)

nCreate :: MonadWeb m => m (Creates a)
nCreate = js $ fn $ \o -> do
  throw "templateCreate not implemented"
  ctx <- createContext o $ "temlpateCreate not implemented"
  retrn ctx

nUpdate :: MonadWeb m => m (Updates a)
nUpdate = js $ fn $ \(_ :: Expr a) (_ :: Expr (Context a)) -> do
  throw "templateUpdate not implemented"
  retrn (Undefined :: Expr ())

nGet :: forall a m. (MonadWeb m, Back (Expr a), Convert (Expr a) ~ Expr a) => m (Gets a)
nGet = js $ fn $ \(_ :: Expr [Node]) -> do
  throw "templateGet not implemented"
  retrn (Undefined :: Expr a)
