{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Monad where

import qualified Data.Text.Lazy as TL

import X.Prelude as P hiding (State, Writer)

import Identifiers as Idents

import qualified CSS as CSS
import qualified CSS.Monad as CSSM
import qualified JS
import DOM

-- ** WebT

declareFields [d|
  data State = State
    { stateJsState :: JS.State
    , stateCssState :: CSSM.State
    }
  |]

declareFields [d|
  data Writer = Writer
    { writerJsCode :: JS.Code ()
    , writerCssCode :: [CSS.Rule]
    }
  |]

declareFields [d|
  data Conf = Conf
    { confJsConf :: JS.Conf
    } deriving (Eq, Show, Read)
  |]

instance Default Conf where
  def = Conf def

instance Default State where
  def = State def cssState
    where
      CSSM.AnimationIdentifiers cssState = def :: CSSM.AnimationIdentifiers

instance Semigroup Writer where
  Writer js css <> Writer js' css' = Writer (js <> js') (css <> css')
instance Monoid Writer where
  mempty = Writer mempty mempty

newtype WebT m a = WebT { unWebT :: RWST Conf Writer State m a }
   deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

-- | The main runner
runWebMT :: Conf -> State -> WebT m a -> m (a, State, Writer)
runWebMT r s wm = runRWST (unWebT wm) r s

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
   getConf :: m Conf
   writeRules
     :: ([CSS.Declaration] -> [CSS.Rule]) -> CSSM.DM a -> m ()

cssF mk m = WebT $ do
  Infinite ident state1 <- gets (^.cssState)
  let
    name = mk ident
    (rules, CSSM.AnimationIdentifiers state2) = CSSM.runCSSM (CSS.selFrom name) (CSSM.AnimationIdentifiers state1) m
  modify (cssState .~ state2)
  tell (mempty & cssCode .~ rules)
  return name

css' = cssF (Class . Static . TL.toStrict)
cssId' = cssF (Id . Static . TL.toStrict)

-- | Main instance
instance (Monad m) => MonadWeb (WebT m) where
   js jsm = WebT $ do
      state0 <- gets (^.jsState)
      let
        JS.State fresh used lib = state0
        ((result, code), state1) = JS.run fresh used lib jsm
      tell $ mempty & jsCode .~ code
      modify' (jsState .~ state1)
      return result
   css = css'
   cssRule sl m = WebT $ do
      tell $ mempty & cssCode .~ CSSM.run sl m
   cssId = cssF (Id . Static . TL.toStrict)
   nextId = WebT $ next cssState
   getState = WebT get
   getConf = WebT ask

   writeRules g dm = WebT $ do
     let ds = execWriter dm :: [CSS.Declaration]
     tell $ mempty & cssCode .~ g ds

fontFace = writeRules (\ds -> [CSS.FontFace ds])

-- ** Instances

-- *** MonadWeb is other if base is other

instance MonadReader r m => MonadReader r (WebT m) where
  ask   = lift ask
  local f (WebT (RWST g)) = WebT $ RWST $ \r s -> local f (g r s)
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
    _ <- pass $ return (a, f)
    return (a, ss, ww)

instance MonadFail m => MonadFail (WebT m) where
  fail _ = undefined

-- ** Helpers

newId :: MonadWeb m => m Id
newId = cssId $ return ()
{-# DEPRECATED newId "Use `cssId (pure ())` instead." #-}
