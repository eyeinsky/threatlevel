module Web.Monad
  ( MonadWeb(..)
  , WebT, runWebMT, run, WebMonadResult
  , jsCode, cssCode

  -- * Helpers
  , webWai, webToResponse
  , newId
  ) where

import Prelude2
import qualified Data.Text.Lazy as TL

import Control.Monad.RWS as RWS

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as E
import Network.Wai

import HTTP.Response

import qualified Web.CSS as CSS
import qualified Web.Browser as Br
import qualified JS
import qualified DOM.JS as JD
import qualified JS.Blaze

import Web.HTML.Blaze


-- ** Web monad

declareLenses [d|
   data State = State
      { jsCounter :: JS.S
      , cssCounter :: Int
      }
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

runWebMT :: Br.Browser -> State -> WebT m a -> m (a, State, Writer)
runWebMT r s wm = RWS.runRWST (unWebT wm) r s

type WebMonadResult m a = m (a, State, Writer)

run :: Br.Browser -> WebT m a -> WebMonadResult m a
run r wm = RWS.runRWST (unWebT wm) r state
   where state = State JS.def 0

-- ** MonadWeb

class Monad m => MonadWeb m where
   js :: JS.M () a -> m a
   css :: CSS.M () -> m CSS.Class
   cssRule :: CSS.SelectorFrom a => a -> CSS.M () -> m ()
   cssId :: CSS.M () -> m CSS.Id

instance (Monad m) => MonadWeb (WebT m) where
   js jsm = WebT $ do
      c <- gets (^.jsCounter)
      b <- ask
      let ((a, code), s') = JS.runM (b, True) c jsm
      tell $ mempty & jsCode .~ code
      modify' (jsCounter .~ s')
      return a
   css m = WebT $ do
      n <- gets (^.cssCounter) <* modify (cssCounter %~ (+ 1))
      let c = CSS.Class $ "c" <> TL.pack (show n)
      tell $ mempty & cssCode .~ CSS.run c m
      return c
   cssRule sl m = WebT $ tell $ mempty & cssCode .~ CSS.run sl m
   cssId m = WebT $ do
      n <- gets (^.cssCounter) <* modify (cssCounter %~ (+ 1))
      let c = CSS.Id $ "i" <> TL.pack (show n)
      tell $ mempty & cssCode .~ CSS.run c m
      return c

-- ** Helpers

webWai :: Monad f => WebT f E.Html -> Request -> f Response
webWai webm req = fmap toWai $ webToResponse browser webm
  where
    hdrs = requestHeaders req
    browser = maybe Br.Unknown Br.parseBrowser . lookup "User-Agent" $ hdrs

webToResponse :: Monad m => Br.Browser -> WebT m E.Html -> m Resp
webToResponse r m = do
   (resp, _, Writer js css) <- run r m
   let reset = CSS.toRules CSS.resetCSS
       addCss = addHead (cssTag . E.toHtml . CSS.pr $ css <> reset)
       addJs = addHead (jsTag $ E.toHtml js)
   addHead (favicon "data:,") . addCss . addJs <$> htmlBody resp

newId = cssId $ return ()
