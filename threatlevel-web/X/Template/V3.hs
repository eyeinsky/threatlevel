{-# LANGUAGE RecordWildCards #-}
module X.Template.V3
  ( module X.Template.V3
  , module X.Template.Common
  ) where

import X.Prelude
import X
import X.Template.Common

-- * API

type Create a = Expr a -> Expr (Context a)
type Update a = Expr a -> Expr (Context a) -> Expr ()
type Mount = Expr ()
type Fields = [Class]


data SSR a out = SSR
  { sSRfields :: Fields
  , sSRSsr :: a -> Html
  , sSROut :: out
  }
makeFields ''SSR

data CSR a out =  Client
  { cSRCreate :: Create a
  , cSRUpdate :: Update a
  , cSRGet :: Expr a
  , cSROut :: Expr a
  }
makeFields ''CSR

data Template a out = Template
  { templateFields :: Fields
  , templateCreate :: Create a
  , templateMount :: Mount
  , templateUpdate :: Update a
  , templateGet :: Expr a

  -- | Both create and ssr map a to the input of html
  , templateSsr :: a -> Html -- ssr

  , templateOut :: out
  }
makeFields ''Template

nTemplate :: forall a m. MonadWeb m => Int -> m (Template a (Out a))
nTemplate n = do
  templateFields <- replicateM n (css $ pure ())
  templateMount <- js $ newf $ do
    throw "templateMount not implemented"
    retrn Undefined
  templateCreate <- js $ fn $ \o -> do
    throw "templateUpdate not implemented"
    ctx <- createContext o $ "temlpateCreate not implemented"
    retrn ctx
  templateUpdate <- js $ fn $ \(_ :: Expr a) (_ :: Expr (Context a)) -> do
    throw "templateUpdate not implemented"
    retrn (Undefined :: Expr ())
  templateGet <- js $ newf $ do
    throw "templateGet not implemented"
    retrn (Undefined :: Expr t)
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
