module Template.V3
  ( module Template.V3
  , module Template.Common
  ) where

import Common.Prelude
import JS
import CSS
import HTML
import Template.Common

-- * API

type Slots = [Class]
type Creates a = Expr a -> Expr (Context a)
type Updates a = Expr a -> Expr (Context a) -> Expr ()
type Gets a = Expr [Node] -> Expr a
type Inits a = Expr (Context a) -> Expr ()

data Template a out get init = Template
  { templateSlots :: Slots
  , templateCreate :: Creates a
  , templateUpdate :: Updates a
  , templateGet :: Gets get
  , templateInit :: init
  , templateOut :: out
  }
makeFields ''Template

type Template' a = Template a (Out a) (Get a) (Init a)

class GetTemplate t where
  type In t :: *
  type In t = ()

  -- | Anything the template needs to pass to outer context.
  type Out t :: *
  type Out t = ()

  type Get t :: *
  type Get t = t

  type Init t :: *
  type Init t = ()

  getTemplate
    :: (Monad m, MonadFix m, JS m)
    => In t -> m (Template t (Out t) (Get t) (Init t))


-- * Helpers

-- ** Slots

-- nSlots :: forall m. JS m => Int -> m Slots
nSlots n = replicateM n (css $ pure ())

-- | Create classes and all field getters
-- nSlotGetters :: JS m => Int -> m (Slots, [Gets Tag])
nSlotGetters n = do
  fields <- nSlots n
  getters <- mapM mkGet fields
  return (fields, getters)

-- | Slots list with typed length
-- nSlotsT :: forall m n. (JS m, NatListC n Class) => Proxy n -> m (NatList n Class)
nSlotsT p = genList p (css $ pure ())

-- nSlotsGettersT
--   :: forall m n.
--    ( JS m
--    , NatListC n Class, Traversable (PeanoList (Prev (ToPeano n))))
--   => Proxy n -> m (NatList n Class, NatList n (Gets Tag))
nSlotsGettersT p = do
  slots <- nSlotsT p
  getters <- mapM mkGet slots
  return (slots, getters)

-- ** Create, update, get

-- nCreate :: JS m => m (Creates a)
nCreate = fn $ \o -> do
  throw "templateCreate not implemented"
  ctx <- createContext o $ "temlpateCreate not implemented"
  return_ ctx

-- nUpdate :: JS m => m (Updates a)
nUpdate = fn $ \(_ :: Expr a) (_ :: Expr (Context a)) -> do
  throw "templateUpdate not implemented"
  return_ (Undefined :: Expr ())

-- nGet :: forall a m. (JS m, Back (Expr a), Convert (Expr a) ~ Expr a) => m (Gets a)
nGet = fn $ \(_ :: Expr [Node]) -> do
  throw "templateGet not implemented"
  return_ (Undefined :: Expr a)

-- * Create into html

createToHtml :: forall a o g i. Template a o g i -> Expr a -> Html
createToHtml template o = let
  ctx = template^.create $ o :: Expr (Context a)
  in dyn $ X.Template.Common.fragment ctx
