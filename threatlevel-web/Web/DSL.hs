module Web.DSL where

import Common.Prelude
import Control.Monad.RWS
import Data.Text.Lazy qualified as TL

import CSS qualified

import JS.DSL.MTL.Mono qualified as JS
import JS.Syntax qualified as JS

import Render
import HTML

{- * Boilerplate

Merge reader/state/writer of MonoJS and MonoCSS by adding apropriate
MTL actions for both.

-}

type Reader_ = (CSS.Reader_, JS.Reader_)
type State_ = (CSS.State_, JS.State_)
type Writer_ = (CSS.Writer_, JS.Writer_)

askCSS :: MonadReader (a, b) m => m a
askCSS = asks fst

askJS :: MonadReader (a1, a2) m => m a2
askJS = asks snd

localCSS :: MonadReader (t, b) m => (t -> t) -> m a -> m a
localCSS f = local $ \(c, j) -> (f c, j)

localJS :: MonadReader (a1, t) m => (t -> t) -> m a2 -> m a2
localJS f = local $ \(c, j) -> (c, f j)

putCSS :: MonadState (a, b) m => a -> m ()
putCSS c' = modify $ \(_, j) -> (c', j)

putJS :: MonadState (a, b) m => b -> m ()
putJS j' = modify $ \(c, _) -> (c, j')

getCSS :: MonadState (a, b) m => m a
getCSS = gets fst

getJS :: MonadState (a1, a2) m => m a2
getJS = gets snd

tellCSS :: (MonadWriter (a, b) m, Monoid b) => a -> m ()
tellCSS t = tell (t, mempty)

tellJS :: (MonadWriter (a, b) m, Monoid a) => b -> m ()
tellJS t = tell (mempty, t)

-- * Mono

type WebRaw m = RWST Reader_ Writer_ State_ m

newtype WebT m a = WebT (WebRaw m a)
  deriving newtype
    ( Functor, Applicative, Monad, MonadTrans, MonadFix, MonadIO
    , MonadReader (CSS.Reader_, JS.Reader_)
    , MonadState (CSS.State_, JS.State_)
    , MonadWriter (CSS.Writer_, JS.Writer_))

runRaw :: WebRaw m a -> Reader_ -> State_ -> m (a, State_, Writer_)
runRaw m r s = runRWST m r s

type Web = WebT Identity

run :: Web a -> Reader_ -> State_ -> (a, State_, Writer_)
run m r s = runRaw (coerce m) r s & runIdentity

hostSelector :: CSS.Selector
hostSelector = CSS.selFrom (CSS.PseudoClass "host" Nothing)

runFresh :: Web a -> (a, State_, Writer_)
runFresh m = run m (hostSelector, JS.Indent 2) (CSS.identifiers, def)

execSubBase'
  :: Monad m
  => m Reader_ -> m State_ -> (State_ -> m a1) -> (t -> m a2) -> (Writer_ -> (b, t)) -> Web a3 -> m (a3, b)
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
  execFunc f = JS.c2 f []

instance CSS.CSS Web where
  css m = CSS.cssNextWith getCSS putCSS CSS.Class m
  cssId m = CSS.cssNextWith getCSS putCSS CSS.Id m
  rule slike m = CSS.ruleBase localCSS tellCSS slike m
  combine f m = CSS.combineBase askCSS f m
  atRule atIdent atCond m = CSS.atRuleBase askCSS tellCSS atIdent atCond m
  execSub m = execSubBase' ask get put tell pick m
    where pick (c, j) = (c, (mempty, j))

instance CSS.Prop Web where
  prop property value = CSS.propBase tellCSS property value

runWeb :: Web a -> (a, TL.Text, TL.Text)
runWeb web = (a, cssText, jsText)
  where
    jsReader = JS.Indent 2
    cssReader = hostSelector
    cssRenderConf = CSS.Pretty 2
    state = (CSS.identifiers, def)

    (a, _, (css, js)) = run web (cssReader, jsReader) state
    css :: CSS.W
    js :: [JS.Statement ()]
    css1 = CSS.wrapW cssReader css :: [CSS.OuterRule]

    cssText = render cssRenderConf css1 :: TL.Text -- todo: or CSS.wrapW (CSS.selFrom CSS.Any) css'
    jsText = render jsReader js :: TL.Text

webHtml :: Web Html -> (Html, Html)
webHtml web = (head', body)
  where
    (body, cssText, jsText) = runWeb web
    head' = do
      HTML.style $ raw cssText
      HTML.script $ raw jsText

runStatic :: Web Html -> HTML.Document
runStatic web = Document $ htmlDoc extra web
  where
    extra = do
      emptyFavicon
      meta ! httpEquiv "Content-Type" ! HTML.content "text/html; charset=utf-8" $ pure ()

-- | Collaps extra head content and @Web Html@ to final @Html@.
--
-- The reason we have @extraHeadContent@ is that from @Web Html@ we
-- extract CSS and JS and add it to head, but we may want to add more
-- stuff to it.
htmlDoc :: Html -> Web Html -> Html
htmlDoc extraHeadContent web = HTML.html $ do
  HTML.head $ headContent_ >> extraHeadContent
  body_
  where
    (headContent_, body_) = webHtml web
