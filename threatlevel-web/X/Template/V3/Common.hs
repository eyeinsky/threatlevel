module X.Template.V3.Common
  ( module X.Template.V3.Common
  , module X.Template.Common
  ) where

import X.Prelude
import X
import X.Template.Common

type Create a = Expr a -> Expr (Context a)
type Update a = Expr a -> Expr (Context a) -> Expr ()
type Mount = Expr ()
type Fields = [Class]

nMount :: MonadWeb m => m (Expr r)
nMount = js $ newf $ do
  log "templateMount not implemented"
  retrn Undefined

nCreate :: MonadWeb m => m (Create a)
nCreate = js $ fn $ \o -> do
  throw "templateCreate not implemented"
  ctx <- createContext o $ "temlpateCreate not implemented"
  retrn ctx

nUpdate :: MonadWeb m => m (Update a)
nUpdate = js $ fn $ \(_ :: Expr a) (_ :: Expr (Context a)) -> do
  throw "templateUpdate not implemented"
  retrn (Undefined :: Expr ())
nGet = js $ newf $ do
  throw "templateGet not implemented"
  retrn (Undefined :: Expr t)
