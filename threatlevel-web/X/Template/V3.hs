{-# LANGUAGE RecordWildCards #-}
module X.Template.V3
  ( module X.Template.V3
  , module X.Template.Common
  ) where

import X.Prelude
import X
import X.Template.Common
import JS.DSL.MTL.Function


-- * API

type Creates a = Expr a -> Expr (Context a)
type Updates a = Expr a -> Expr (Context a) -> Expr ()
type Gets a = Expr [Node] -> Expr a
type Slots = [Class]

data Template a out get = Template
  { templateSlots :: Slots
  , templateCreate :: Creates a
  , templateUpdate :: Updates a
  , templateGet :: Gets get
  , templateOut :: out
  }
makeFields ''Template

type Template' a = Template a (Out a) (Get a)

class GetTemplate t where
  type In t :: *
  type In t = ()

  -- | Anything the template needs to pass to outer context.
  type Out t :: *
  type Out t = ()

  type Get t :: *
  type Get t = t

  getTemplate
    :: (Monad m, MonadFix m)
    => In t -> WebT m (Template t (Out t) (Get t))


-- * Helpers

-- ** Slots

nSlots :: forall m. MonadWeb m => Int -> m Slots
nSlots n = replicateM n (css $ pure ())

-- | Create classes and all field getters
nSlotGetters :: MonadWeb m => Int -> m (Slots, [Gets Tag])
nSlotGetters n = do
  fields <- nSlots n
  getters <- mapM mkGet fields
  return (fields, getters)

-- ** Create, update, get

nCreate :: MonadWeb m => m (Creates a)
nCreate = js $ fn $ \o -> do
  throw "templateCreate not implemented"
  ctx <- createContext o $ "temlpateCreate not implemented"
  return_ ctx

nUpdate :: MonadWeb m => m (Updates a)
nUpdate = js $ fn $ \(_ :: Expr a) (_ :: Expr (Context a)) -> do
  throw "templateUpdate not implemented"
  return_ (Undefined :: Expr ())

nGet :: forall a m. (MonadWeb m, Back (Expr a), Convert (Expr a) ~ Expr a) => m (Gets a)
nGet = js $ fn $ \(_ :: Expr [Node]) -> do
  throw "templateGet not implemented"
  return_ (Undefined :: Expr a)
