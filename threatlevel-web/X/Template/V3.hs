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
  { sSRfields :: Fields
  , sSRSsr :: a -> Html
  , sSROut :: out
  }
makeFields ''SSR

data Template a out = Template
  { templateFields :: Fields
  , templateCreate :: Create a
  , templateMount :: Mount
  , templateUpdate :: Update a
  , templateGet :: Get a

  -- | Both create and ssr map a to the input of html
  , templateSsr :: a -> Html -- ssr

  , templateOut :: out
  }
makeFields ''Template

nTemplate :: forall a m. MonadWeb m => Int -> m (Template a (Out a))
nTemplate n = do
  templateFields <- replicateM n (css $ pure ())
  templateMount <- nMount
  templateCreate <- nCreate
  templateUpdate <- nUpdate
  templateGet <- nGet
  let
    templateSsr _ = error "SSR not implemented"
    templateOut = error "Out not implemented"
  return Template{..}

class GetTemplate t where
  type In t :: *
  type In t = ()

  -- | Anything the template needs to pass to outer context.
  type Out t :: *
  type Out t = ()

  getTemplate :: (Monad m, MonadFix m) => In t -> WebT m (Template t (Out t))
