module Web.Endpoint where

import Pr
import qualified Data.Text as T

import qualified Web as W
import HTTP.Response
import IdentifierSource

import JS
import DOM.JS

-- * Endpoint

-- ** EndpointT

type Inner m r = W.WebT (ReaderT r m)

type Url = T.Text
type Urls = [Url]
type Endpoints m = [(Url, m Raw)]
newtype EndpointT m a = EndpointT
  { unEndpointT :: RWST (Url -> Url) (Endpoints m) Urls m a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

deriving instance MonadFix m => MonadFix (W.WebT m)

runEndpointT :: (Url -> Url) -> EndpointT m a -> m (a, Urls, Endpoints m)
runEndpointT url m = runRWST (unEndpointT m) url identifierSource

-- ** MonadEndpoint

class (Monad m) => MonadEndpoint m where
  type Sub m :: * -> *
  endpoint :: ToRaw r => Sub m r -> m Url
  pinned :: ToRaw r => Url -> Sub m r -> m Url
  urlMaker :: m (Url -> Url)

instance (Monad m) => MonadEndpoint (EndpointT m) where
  type Sub (EndpointT m) = m
  endpoint m = EndpointT $ do
    p <- next
    tell [(p, toRaw <$> m)]
    ask <*> pure p
    where
      next :: forall m. MonadState Urls m => m Url
      next = get >>= \(x : xs) -> put xs *> return x
  pinned url m = EndpointT $ do
    tell [(url, toRaw <$> m)]
    ask <*> pure url
  urlMaker = EndpointT ask

currentUrl = urlMaker <&> ($ "")

-- ** Instances

instance MonadTrans EndpointT where
  lift m = EndpointT $ RWST $ \_ s -> do
    a <- m
    return (a, s, mempty)

instance W.MonadWeb m => W.MonadWeb (EndpointT m) where
  js = lift . W.js
  css = lift . W.css
  cssRule a b = lift $ W.cssRule a b
  cssId = lift . W.cssId
  nextId = lift $ W.nextId
  getState = lift W.getState

instance Monad m => MonadReader r (EndpointT (Inner m r)) where
  ask = lift . lift $ ask
  local = todo -- but functionality is not used anywhere, so ok for now
  reader = lift . lift . reader

-- * Extensions

endpoint' m = W.js . newf . xhrJs "post" . ulit =<< endpoint m
