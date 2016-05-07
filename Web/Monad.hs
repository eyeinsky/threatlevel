module Web.Monad where

import Prelude2
import qualified Data.Text.Lazy as TL

import Control.Monad.RWS as RWS

import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html5            as E

import HTTP_Response

import qualified Web_CSS as CSS
import qualified Web as W
import qualified JS
import qualified JS.Monad as JM
import qualified JS_DOM as JD
import qualified JS_Blaze

import Web.Blaze


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

newtype WebT m a = WebT { runWebT :: RWS.RWST W.Browser Writer State m a }
   deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runWebMT :: W.Browser -> State -> WebT m a -> m (a, State, Writer)
runWebMT r s wm = RWS.runRWST (runWebT wm) r s

run r wm = RWS.runRWST (runWebT wm) r state
   where state = State JS.def 0

-- ** MonadWeb

class Monad m => MonadWeb m where
   js :: JS.M () a -> m a
   css :: CSS.DM () -> m CSS.Class

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
      return cls

-- ** Helpers

webToResponse r m = do
   (resp, _ {-states-}, Writer js css) <- run r m
   let cssX = css <> CSS.toRules CSS.resetCSS
       addJs = addHead (jsTag $ E.toHtml js)
       addCss = addHead (cssTag . E.toHtml . CSS.pr $ cssX)
   addHead (favicon "") . addCss . addJs <$> htmlBody resp
