module CSS.Monad where

import qualified Data.Text.Lazy as TL
import Control.Monad.Reader
import Control.Monad.State (evalStateT)
import Control.Monad.Writer
import Control.Monad.Identity

import Identifiers as Idents

import Pr
import Web.Browser
import CSS.Internal
import qualified CSS.MediaQuery as MediaQuery


-- * DSL setup

declareFields [d|
  data R = R
    { rSelector :: Selector
    , rBrowser :: Browser
    }
  |]

declareFields [d|
  data State = State
    { stateIdents :: [TL.Text]
    , stateAnimationIdents :: [TL.Text]
    }
  |]

declareFields [d|
  data CSSW = CSSW
    { cSSWRules :: [Rule]
    , cSSWDecls :: [Declaration]
    }
  |]

instance Default State where
  def = State idents' animationIdents'
    where
      idents' = map TL.fromStrict Idents.identifierSource
      animationIdents' = Idents.identifiersFilter forbidden <&> TL.fromStrict
      forbidden = ["none", "unset", "initial", "inherit"]
      -- ^ As by https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name

type DM = Writer [Declaration]

type CSSM = WriterT CSSW (StateT State (ReaderT R Identity))

-- * For export

type M = CSSM

run :: SelectorFrom a => Browser -> a -> CSSM () -> [Rule]
run b s m = runCSSM (R (selFrom s) b) def m & fst

rule :: SelectorFrom a => a -> DM () -> CSSM ()
rule s ds = tellRules $ pure $ mkRule (selFrom s) (execWriter ds)

prop
  :: (HasDecls w [Declaration], MonadWriter w m)
  => TL.Text -> Value -> m ()
prop k v = tellDecls $ pure $ mkDeclaration k v

tellRules rs = tell $ mempty & rules .~ rs
tellRule =  tellRules . pure

tellDecls ds = tell $ mempty & decls .~ ds

runCSSM :: R -> State -> CSSM () -> ([Rule], State)
runCSSM r s m = (r' : cssw^.rules, state)
  where
    a0 = execWriterT m
    a1 = runStateT a0 s
    (cssw, state) = runReader a1 r
    r' = mkRule (r^.selector) (cssw^.decls)

instance Monoid CSSW where
  mempty = CSSW mempty mempty
  mappend a b = CSSW (a^.rules <> b^.rules) (a^.decls <> b^.decls)

apply :: (SimpleSelector -> SimpleSelector) -> Selector -> Selector
apply f s = go s
  where
    go :: Selector -> Selector
    go s = case s of
      Simple ss -> Simple (f ss)
      Combined op s ss -> Combined op s (f ss)

pseudo :: TL.Text -> CSSM () -> CSSM ()
pseudo t m = do
  conf <- ask
  let hoovered = apply (pseudo' t) (conf^.selector)
  tellRules' (R hoovered $ conf^.browser) m
  where
    pseudo' :: TL.Text -> SimpleSelector -> SimpleSelector
    pseudo' t s = s & pseudos %~ (Pseudo t:)

combinator :: SOp -> SimpleSelector -> CSSM () -> CSSM ()
combinator c d m = do
  conf <- ask
  let r = R (Combined c (conf^.selector) d) (conf^.browser)
  tellRules' r m

-- | Tell rules and thread state
tellRules' r m = do
  state0 <- get
  let (rules, state1) = runCSSM r state0 m
  put state1
  tellRules rules

-- * Keyframe monad

declareFields [d|
  data DeclW = DeclW { declWDecls :: [Declaration] }
  |]
-- ^ The 'decls' lens needed to reuse all the shorthands.

instance Monoid DeclW where
  mempty = DeclW mempty
  mappend a b = DeclW (a^.decls <> b^.decls)

type DeclM = WriterT DeclW (Reader Browser)
execDeclM :: Browser -> DeclM a ->  DeclW
execDeclM r dm = runReader (execWriterT dm) r

type KM = WriterT [KeyframeBlock] (Reader Browser)

keyframe :: Int -> DeclM () -> KM ()
keyframe n dm = do
  dw <- ask <&> runReader (execWriterT dm)
  tell $ pure $ KeyframeBlock (KPercent n) (dw^.decls)

keyframes :: KM () -> CSSM Value
keyframes km = do
  name <- Idents.next animationIdents
  ks <- asks (view browser) <&> runReader (execWriterT km)
  tellRule $ Keyframes name ks
  return $ Word name

-- * Media query

media :: TL.Text -> CSSM () -> CSSM ()
media e dm = do
  conf <- ask
  state0 <- get
  let (rules, state1) = runCSSM conf state0 dm
  tellRule $ MediaQuery expr rules
  put state1
  where
    expr = MediaQuery.Expr e
