module Web.Monad
  ( MonadWeb(..)
  , WebT, runWebMT, run, WebMonadResult
  , jsCode, cssCode
  ) where

import Prelude2
import qualified Data.Text.Lazy as TL

import Control.Monad.RWS as RWS

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as E

import HTTP.Response

import qualified Web.CSS as CSS
import qualified Web as W
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

newtype WebT m a = WebT { unWebT :: RWS.RWST W.Browser Writer State m a }
   deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }
instance MonadReader r m => MonadReader r (WebT m) where
  ask   = lift ask
  local f (WebT (RWS.RWST g)) = WebT $ RWS.RWST $ \r s -> do
    r' <- ask
    local f (g r s)
  reader = lift . reader

instance MonadState s m => MonadState s (WebT m) where
  state f = lift $ state f -- state :: (s -> (a, s)) -> m a

instance MonadWriter w m => MonadWriter w (WebT m) where
  tell w = lift $ tell w
  listen (WebT (RWST f)) = WebT $ RWST $ \r s -> do
    ((a, ss, ww), w) <- listen (f r s)
    return ((a, w), ss, ww)
  pass (WebT (RWST f)) = WebT $ RWST $ \r s -> do
    ((a, f :: w -> w), ss, ww) <- f r s
    pass $ return (a, f)
    return (a, ss, ww)

runWebMT :: W.Browser -> State -> WebT m a -> m (a, State, Writer)
runWebMT r s wm = RWS.runRWST (unWebT wm) r s

type WebMonadResult m a = m (a, State, Writer)

run :: W.Browser -> WebT m a -> WebMonadResult m a
run r wm = RWS.runRWST (unWebT wm) r state
   where state = State JS.def 0

-- ** MonadWeb

class Monad m => MonadWeb m where
   js :: JS.M () a -> m a
   css :: CSS.DM () -> m CSS.Class
   cssRule :: CSS.SelectorFrom a => a -> CSS.DM () -> m ()
   cssId :: CSS.DM () -> m CSS.Id

instance (Monad m) => MonadWeb (WebT m) where
   js jsm = WebT $ do
      c <- gets (^.jsCounter)
      b <- ask
      let ((a, code), s') = JS.runM (b, True) c jsm
      tell $ mempty & jsCode .~ code
      modify' (jsCounter .~ s')
      return a
   css decm = WebT $ do
      c <- gets (^.cssCounter)
      let cls = CSS.Class $ "c" <> TL.pack (show c)
          (a, rules) = CSS.runRM $ CSS.rule cls decm
      tell $ mempty & cssCode .~ rules
      c <- modify (cssCounter %~ (+ 1))
      return cls
   cssRule selectorLike decm = WebT $ do
      let selector = W.selFrom selectorLike
          (a, rules) = CSS.runRM $ CSS.rule selector decm
      tell $ mempty & cssCode .~ rules
      return ()
   cssId decm = WebT $ do
      c <- gets (^.cssCounter)
      let cls = CSS.Id $ "i" <> TL.pack (show c)
          (a, rules) = CSS.runRM $ CSS.rule cls decm
      tell $ mempty & cssCode .~ rules
      c <- modify (cssCounter %~ (+ 1))
      return cls

-- ** Helpers

webToResponse r m = do
   (resp, _ {-states-}, Writer js css) <- run r m
   let cssX = css <> CSS.toRules CSS.resetCSS
       addJs = addHead (jsTag $ E.toHtml js)
       addCss = addHead (cssTag . E.toHtml . CSS.pr $ cssX)
   addHead (favicon "") . addCss . addJs <$> htmlBody resp
