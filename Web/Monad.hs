module Web.Monad where

import Prelude2
import qualified Data.Text.Lazy as TL
import Data.Default

import Control.Monad.RWS as RWS
import qualified Control.Monad.Writer as MW
import qualified Control.Monad.State as MS

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as E
import Network.Wai

import HTTP.Response

import qualified Web.CSS as CSS
import qualified Web.CSS.Monad as CSSM
import qualified Web.Browser as Br
import qualified JS
import qualified DOM.JS as JD
import qualified JS.Blaze

import DOM.Internal
import Web.HTML.Blaze
import Render

-- ** WebT

declareLenses [d|
   data State = State
      { jsCounter :: JS.S
      , cssCounter :: Int
      }

   instance Default State where
      def = State def 0

   data Writer = Writer
      { jsCode :: JS.Code ()
      , cssCode :: [CSS.Rule]
      }

   instance Monoid Writer where
      mempty = Writer mempty mempty
      mappend (Writer js css) (Writer js' css') = Writer (js <> js') (css <> css')
   |]

newtype WebT m a = WebT { unWebT :: RWS.RWST Br.Browser Writer State m a }
   deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runWebMT :: Br.Browser -> State -> WebT m a -> m (a, State, Writer)
runWebMT r s wm = RWS.runRWST (unWebT wm) r s

run :: Br.Browser -> WebT m a -> WebMonadResult m a
run r wm = RWS.runRWST (unWebT wm) r state
   where state = State def 0

type WebMonadResult m a = m (a, State, Writer)

-- * MonadWeb

class Monad m => MonadWeb m where
   js :: JS.M () a -> m a
   css :: CSSM.M () -> m CSS.Class
   cssRule :: CSS.SelectorFrom a => a -> CSSM.M () -> m ()
   cssId :: CSSM.M () -> m CSS.Id
   nextId :: m Int

-- | Main instance
instance (Monad m) => MonadWeb (WebT m) where
   js jsm = WebT $ do
      c <- gets (^.jsCounter)
      b <- ask
      let ((a, code), s') = JS.runM (b, True) c jsm
      tell $ mempty & jsCode .~ code
      modify' (jsCounter .~ s')
      return a
   css m = WebT $ do
      b <- ask
      n <- gets (^.cssCounter) <* modify (cssCounter %~ (+ 1))
      let c = CSS.Class $ Static $ "c" <> TL.pack (show n)
      tell $ mempty & cssCode .~ CSSM.run b c m
      return c
   cssRule sl m = WebT $ do
      b <- ask
      tell $ mempty & cssCode .~ CSSM.run b sl m
   cssId m = WebT $ do
      b <- ask
      n <- gets (^.cssCounter) <* modify (cssCounter %~ (+ 1))
      let c = CSS.Id $ Static $ "i" <> TL.pack (show n)
      tell $ mempty & cssCode .~ CSSM.run b c m
      return c
   nextId = WebT $ gets (^.cssCounter)

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

instance (MonadWeb m) => MonadWeb (MS.StateT s m) where
  js = lift . js
  css = lift . css
  cssRule a b = lift $ cssRule a b
  cssId = lift . cssId
  nextId = lift $ nextId

-- ** Helpers

webWai :: Monad f => WebT f E.Html -> Request -> f Response
webWai webm req = fmap toWai $ webToResponse browser webm
  where
    hdrs = requestHeaders req
    browser = maybe Br.Unknown Br.parseBrowser . lookup "User-Agent" $ hdrs

webToResponse :: Monad m => Br.Browser -> WebT m E.Html -> m Resp
webToResponse b m = do
   (resp, _, Writer js css) <- run b m
   let addCss = addHead (cssTag . E.toHtml . render $ css <> CSS.resetCSS b)
       addJs = addHead (jsTag $ E.toHtml js)
   addHead (favicon "data:;base64,iVBORw0KGgo=") . addCss . addJs <$> htmlBody resp

newId = cssId $ return ()
