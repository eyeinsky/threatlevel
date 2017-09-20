{-# LANGUAGE UndecidableInstances #-}
module Web.Monad where

import Pr
import qualified Data.Text.Lazy as TL
import Data.Default

import Control.Monad.RWS as RWS
import qualified Control.Monad.Writer as MW
import qualified Control.Monad.State as MS

import Network.Wai

import HTTP.Response

import qualified CSS as CSS
import qualified CSS.Monad as CSSM
import qualified Web.Browser as Br
import qualified JS

import DOM
import Render

import qualified Identifiers as IS

-- ** WebT

declareFields [d|
  data State = State
    { stateJsState :: JS.State
    , stateCssState :: [TL.Text]
    }
  |]

declareFields [d|
  data Writer = Writer
    { writerJsCode :: JS.Code ()
    , writerCssCode :: [CSS.Rule]
    }
  |]

type Conf = JS.Config

instance Default State where
  def = State def def

instance Monoid Writer where
  mempty = Writer mempty mempty
  mappend (Writer js css) (Writer js' css') = Writer (js <> js') (css <> css')

newtype WebT m a = WebT { unWebT :: RWS.RWST Conf Writer State m a }
   deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

-- | The main runner
runWebMT :: Conf -> State -> WebT m a -> m (a, State, Writer)
runWebMT r s wm = RWS.runRWST (unWebT wm) r s

foldWM :: Foldable t
  => Conf
  -> State
  -> t (WebT Identity a) -> ([a], JS.Code (), [CSS.Rule])
foldWM r st ws = foldl step (st, []) ws & snd & final
  where
    runOne s = runIdentity . runWebMT r s
    step (s, li) m = let
        (a, s', w) = runOne s m
      in (s', (a, w) : li)
    final li = let
      (htmls, ws) = unzip li
      js = view jsCode <$> ws & mconcat
      css = view cssCode <$> ws & mconcat
      in (reverse htmls, js, css)

type WebMonadResult m a = m (a, State, Writer)

-- * MonadWeb

class Monad m => MonadWeb m where
   js :: JS.M () a -> m a
   css :: CSSM.M () -> m Class
   cssRule :: CSS.SelectorFrom a => a -> CSSM.M () -> m ()
   cssId :: CSSM.M () -> m Id
   nextId :: m TL.Text
   getState :: m State

askBrowser = asks (^.Br.browser)

-- | Main instance
instance (Monad m) => MonadWeb (WebT m) where
   js jsm = WebT $ do
      c <- gets (^.jsState)
      config <- ask
      let
        ((a, code), s') = JS.runM config c jsm
      tell $ mempty & jsCode .~ code
      modify' (jsState .~ s')
      return a
   css m = WebT $ do
      b <- askBrowser
      c <- Class . Static <$> IS.next cssState
      tell $ mempty & cssCode .~ CSSM.run b c m
      return c
   cssRule sl m = WebT $ do
      b <- askBrowser
      tell $ mempty & cssCode .~ CSSM.run b sl m
   cssId m = WebT $ do
      b <- askBrowser
      c <- Id . Static <$> IS.next cssState
      tell $ mempty & cssCode .~ CSSM.run b c m
      return c
   nextId = WebT $ Pr.head <$> gets (^.cssState)
   getState = WebT get

-- ** Instances

-- *** MonadWeb is other if base is other

instance MonadReader r m => MonadReader r (WebT m) where
  ask   = lift ask
  local f (WebT (RWS.RWST g)) = WebT $ RWS.RWST $ \r s -> do
    r' <- ask
    local f (g r s)
  reader = lift . reader

instance MonadState s m => MonadState s (WebT m) where
  state f = lift $ state f

instance MonadWriter w m => MonadWriter w (WebT m) where
  tell w = lift $ tell w
  listen (WebT (RWST f)) = WebT $ RWST $ \r s -> do
    ((a, ss, ww), w) <- listen (f r s)
    return ((a, w), ss, ww)
  pass (WebT (RWST f)) = WebT $ RWST $ \r s -> do
    ((a, f :: w -> w), ss, ww) <- f r s
    pass $ return (a, f)
    return (a, ss, ww)

-- *** Other is MonadWeb if base is MonadWeb

instance (MonadWeb m, Monoid w) => MonadWeb (MW.WriterT w m) where
  js = lift . js
  css = lift . css
  cssRule a b = lift $ cssRule a b
  cssId = lift . cssId
  nextId = lift $ nextId
  getState = lift getState

instance (MonadWeb m) => MonadWeb (MS.StateT s m) where
  js = lift . js
  css = lift . css
  cssRule a b = lift $ cssRule a b
  cssId = lift . cssId
  nextId = lift $ nextId
  getState = lift getState

-- ** Helpers

newId :: MonadWeb m => m Id
newId = cssId $ return ()
