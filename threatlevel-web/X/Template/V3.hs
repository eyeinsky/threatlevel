{-# LANGUAGE RecordWildCards #-}
module X.Template.V3
  ( module X.Template.V3
  , module X.Template.V3.Common
  ) where

import X.Prelude
import X
import X.Template.V3.Common

-- * API

data SSR a out = SSR
  { sSRfields :: Slots
  , sSRSsr :: a -> Html
  , sSROut :: out
  }
makeFields ''SSR

data Template a out = Template
  { templateSlots :: Slots
  , templateCreate :: Creates a
  , templateUpdate :: Updates a
  , templateGet :: Gets a

  -- | Both create and ssr map a to the input of html
  , templateSsr :: a -> Html -- ssr

  , templateOut :: out
  }
makeFields ''Template


class GetTemplate t where
  type In t :: *
  type In t = ()

  -- | Anything the template needs to pass to outer context.
  type Out t :: *
  type Out t = ()

  getTemplate :: (Monad m, MonadFix m) => In t -> WebT m (Template t (Out t))
