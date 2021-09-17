{-# LANGUAGE RecordWildCards #-}
module X.Template.V3 where

import X.Prelude
import X
import X.Template.Common

-- * API

type Create a = Expr a -> Expr (Context a)
type Update a = Expr a -> Expr ()
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
  , templateUpdate :: Update a
  , templateGet :: Expr a

  -- | Both create and ssr map a to the input of html
  , templateSsr :: a -> Html -- ssr

  , templateOut :: out
  }
makeFields ''Template

nTemplate :: MonadWeb m => Int -> m (Template a ())
nTemplate n = do
  templateFields <- replicateM n (css $ pure ())
  templateCreate <- js $ fn $ \o -> do
    throw "templateUpdate not implemented"
    ctx <- createContext o $ "temlpateCreate not implemented"
    retrn ctx
  templateUpdate <- js $ fn $ \(_ :: Expr t) -> do
    throw "templateUpdate not implemented"
    retrn (Undefined :: Expr ())
  templateGet <- js $ newf $ do
    throw "templateGet not implemented"
    retrn (Undefined :: Expr t)
  let
    templateSsr _ = error "SSR not implemented"
    templateOut = ()
  return Template{..}
