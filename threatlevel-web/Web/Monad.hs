{-# LANGUAGE NoDeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Monad where

import qualified Data.Text.Lazy as TL

import X.Prelude as P hiding (State, Writer)

import Identifiers as Idents

import qualified CSS as CSS
import qualified CSS.DSL as CSSM
import qualified JS
import DOM

-- ** WebT

newtype AnimationIdentifiers
  = AnimationIdentifiers (Infinite TL.Text)
  deriving newtype (Increment)
instance Default AnimationIdentifiers where
  def = AnimationIdentifiers CSS.identifiersLazy

declareFields [d|
  data State = State
    { stateJsState :: JS.State
    , stateCssState :: CSSM.Names
    , stateAnimationIdentifiers :: AnimationIdentifiers
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
  def = State def CSS.identifiersLazy def

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
   writeRules
     :: ([CSS.Declaration] -> [CSS.Rule]) -> CSSM.DM a -> m ()

cssF :: (Monad m, CSS.SelectorFrom a) => (TL.Text -> a) -> CSSM.CSSM -> WebT m a
cssF mk m = WebT $ do
  name <- mk <$> next cssState
  tell (mempty & cssCode .~ CSSM.rulesFor name m)
  return name

css' :: Monad m => CSSM.CSSM -> WebT m Class
css' = cssF (Class . Static . TL.toStrict)

cssId' :: Monad m => CSSM.CSSM -> WebT m Id
cssId' = cssF (Id . Static . TL.toStrict)

-- | Main instance
instance (Monad m) => MonadWeb (WebT m) where
   js jsm = WebT $ do
      jsRenderConf <- asks (^.jsConf)
      state0 <- gets (^.jsState)
      let
        JS.State fresh used lib = state0
        ((result, code), state1) = JS.run jsRenderConf lib used fresh jsm
      tell $ mempty & jsCode .~ code
      modify' (jsState .~ state1)
      return result
   css = css'
   cssRule sl m = WebT $ do
      tell $ mempty & cssCode .~ CSSM.rulesFor sl m
   cssId = cssF (Id . Static . TL.toStrict)
   nextId = WebT $ next cssState
   getState = WebT get

   writeRules g dm = WebT $ do
     let ds = execWriter dm :: [CSS.Declaration]
     tell $ mempty & cssCode .~ g ds

keyframes :: Monad m => CSSM.KM () -> WebT m CSS.Value
keyframes km = WebT $ do
  name <- next animationIdentifiers
  let (name', rule) = CSSM.keyframes' name km
  tell $ mempty & cssCode .~ pure rule
  return name'

fontFace :: MonadWeb m => CSSM.DM a -> m ()
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
