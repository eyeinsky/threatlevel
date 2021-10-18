module X.Template.V3.CSR where

import X.Prelude
import X
import X.Template.V3.Common

data CSR a out =  CSR
  { cSRFields :: Fields
  , cSRMount :: Mount
  , cSRCreate :: Create a
  , cSRUpdate :: Update a
  , cSRGet :: Get a
  , cSROut :: Expr a
  }
makeFields ''CSR

class GetCSR t where
  type In t :: *
  type In t = ()

  -- | Anything the template needs to pass to outer context.
  type Out t :: *
  type Out t = ()

  getCSR :: (Monad m, MonadFix m) => In t -> WebT m (CSR t (Out t))

nCSR :: forall a m. MonadWeb m => Int -> m (CSR a (Out a))
nCSR n = do
  cSRFields <- replicateM n (css $ pure ())
  cSRMount <- nMount
  cSRCreate <- nCreate
  cSRUpdate <- nUpdate
  cSRGet <- nGet
  let cSROut = Undefined
  return $ CSR{..}
