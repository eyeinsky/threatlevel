module Web.DSL where

import Common.Prelude
import Control.Monad.RWS

import qualified CSS.DSL.MTL.Mono as CSS
import qualified CSS.Identifiers as CSS

import Data.Text.Lazy qualified as TL

-- import qualified CSS.DSL.MTL.Mono as CSS
-- import qualified CSS.Identifiers as CSS
import CSS qualified
import qualified JS.DSL.MTL.Mono as JS
import qualified JS.Syntax as JS

import Render

{- * Boilerplate

Merge reader/state/writer of MonoJS and MonoCSS by adding apropriate
MTL actions for both.

-}

type Reader_ = (CSS.Reader_, JS.Reader_)
type State_ = (CSS.State_, JS.State_)
type Writer_ = (CSS.Writer_, JS.Writer_)

askCSS = asks fst
askJS = asks snd
localCSS f = local $ \(c, j) -> (f c, j)
localJS f = local $ \(c, j) -> (c, f j)
putCSS c' = modify $ \(_, j) -> (c', j)
putJS j' = modify $ \(c, _) -> (c, j')
getCSS = gets fst
getJS = gets snd
tellCSS t = tell (t, mempty)
tellJS t = tell (mempty, t)

-- * Mono

type WebRaw m = RWST Reader_ Writer_ State_ m

newtype WebT m a = WebT (WebRaw m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans)
  deriving (MonadReader (CSS.Reader_, JS.Reader_)) via WebRaw m
  deriving (MonadState (CSS.State_, JS.State_)) via WebRaw m
  deriving (MonadWriter (CSS.Writer_, JS.Writer_)) via WebRaw m
  deriving MonadFix via WebRaw m

runRaw :: WebRaw m a -> Reader_ -> State_ -> m (a, State_, Writer_)
runRaw m r s = runRWST m r s

type Web = WebT Identity

run :: Web a -> Reader_ -> State_ -> (a, State_, Writer_)
run m r s = runRaw (coerce m) r s & runIdentity

host = CSS.PseudoClass "host" Nothing

runFresh :: Web a -> (a, State_, Writer_)
runFresh m = run m (CSS.selFrom host, JS.Indent 2) (CSS.identifiers, def)

execSubBase' ask get put tell pick m = do
  (a, s, w) <- run m <$> ask <*> get
  let (toReturn, toTell) = pick w
  tell toTell *> put s $> (a, toReturn)

instance JS.JS Web where
  stm s = tellJS (pure s)
  freshName = JS.freshNameBase getJS putJS
  bind = JS.bindBase
  execSub m = execSubBase' ask get put tell pick m
    where pick (c, j) = (j, (c, mempty))

  -- f1 :: forall f . (C1 f, MonadFor f ~ m) => f -> m (Expr ())
  f1 = error "Web.DSL: f1 not defined"
  f2 = JS.f2Base JS.bind
  f3 = error "Web.DSL: f3 not defined"
  -- f3 :: forall f . (C3 f m) => f -> m RetUntyped

instance CSS.CSS Web where
  css m = CSS.cssBase getCSS putCSS m
  rule slike m = CSS.ruleBase localCSS tellCSS slike m
  combine f m = CSS.combineBase askCSS f m
  atRule atIdent atCond m = CSS.atRuleBase askCSS tellCSS atIdent atCond m
  execSub m = execSubBase' ask get put tell pick m
    where pick (c, j) = (c, (mempty, j))

instance CSS.Prop Web where
  prop property value = CSS.propBase tellCSS property value

instance Render (Web a) where
  type Conf (Web a) = (CSS.Conf, JS.Conf)
  renderM m = pure text
    where
      (css2, js1) = renderWeb m
      text = "<style>\n"
          <> css2
          <> "</style>\n"
          <> "<script>\n"
          <> js1
          <> "</script>"

renderWeb :: Web a -> (TL.Text, TL.Text)
renderWeb m = (css2, js1)
  where
    (_, _, (css, js)) = runFresh m
    css1 = CSS.wrapW (CSS.selFrom host) css
    css2 = render (CSS.Pretty 2) css1
    js1 = render (JS.Indent 2) js

-- foldWM :: Foldable t
--   => Conf
--   -> State
--   -> t (WebT Identity a) -> ([a], JS.Code (), [CSS.Rule])
-- foldWM r st ws = foldl step (st, []) ws & snd & final
--   where
--     runOne s = runIdentity . runWebMT r s
--     step (s, li) m = let
--         (a, s', w) = runOne s m
--       in (s', (a, w) : li)
--     final li = let
--       (htmls, ws) = unzip li
--       js = view jsCode <$> ws & mconcat
--       css = view cssCode <$> ws & mconcat
--       in (reverse htmls, js, css)

-- -- * MonadWeb

-- class Monad m => MonadWeb m where
--    js :: JS.M () a -> m a
--    css :: CSSM.M () -> m Class
--    cssRule :: CSS.SelectorFrom a => a -> CSSM.M () -> m ()
--    cssId :: CSSM.M () -> m Id
--    nextId :: m TL.Text
--    getState :: m State
--    writeRules
--      :: ([CSS.Declaration] -> [CSS.Rule]) -> CSSM.DM a -> m ()

-- cssF :: (Monad m, CSS.SelectorFrom a) => (TL.Text -> a) -> CSSM.CSSM -> WebT m a
-- cssF mk m = WebT $ do
--   name <- mk <$> next cssState
--   tell (mempty & cssCode .~ CSSM.rulesFor name m)
--   return name

-- css' :: Monad m => CSSM.CSSM -> WebT m Class
-- css' = cssF (Class . Static . TL.toStrict)

-- cssId' :: Monad m => CSSM.CSSM -> WebT m Id
-- cssId' = cssF (Id . Static . TL.toStrict)

-- -- | Main instance
-- instance (Monad m) => MonadWeb (WebT m) where
--    js jsm = WebT $ do
--       jsRenderConf <- asks (^.jsConf)
--       state0 <- gets (^.jsState)
--       let
--         JS.State fresh used lib = state0
--         ((result, code), state1) = JS.run jsRenderConf lib used fresh jsm
--       tell $ mempty & jsCode .~ code
--       modify' (jsState .~ state1)
--       return result
--    css = css'
--    cssRule sl m = WebT $ do
--       tell $ mempty & cssCode .~ CSSM.rulesFor sl m
--    cssId = cssF (Id . Static . TL.toStrict)
--    nextId = WebT $ next cssState
--    getState = WebT get

--    writeRules g dm = WebT $ do
--      let ds = execWriter dm :: [CSS.Declaration]
--      tell $ mempty & cssCode .~ g ds

-- keyframes :: Monad m => CSSM.KM () -> WebT m CSS.Value
-- keyframes km = WebT $ do
--   name <- next animationIdentifiers
--   let (name', rule) = CSSM.keyframes' name km
--   tell $ mempty & cssCode .~ pure rule
--   return name'

-- fontFace :: MonadWeb m => CSSM.DM a -> m ()
-- fontFace = writeRules (\ds -> [CSS.FontFace ds])

-- -- ** Instances

-- -- *** MonadWeb is other if base is other

-- instance MonadReader r m => MonadReader r (WebT m) where
--   ask   = lift ask
--   local f (WebT (RWST g)) = WebT $ RWST $ \r s -> local f (g r s)
--   reader = lift . reader

-- instance MonadState s m => MonadState s (WebT m) where
--   state f = lift $ state f

-- instance MonadWriter w m => MonadWriter w (WebT m) where
--   tell w = lift $ tell w
--   listen (WebT (RWST f)) = WebT $ RWST $ \r s -> do
--     ((a, ss, ww), w) <- listen (f r s)
--     return ((a, w), ss, ww)
--   pass (WebT (RWST f)) = WebT $ RWST $ \r s -> do
--     ((a, f :: w -> w), ss, ww) <- f r s
--     _ <- pass $ return (a, f)
--     return (a, ss, ww)

-- instance MonadFail m => MonadFail (WebT m) where
--   fail _ = undefined
