module CSS.Monad where

import qualified Data.Text.Lazy as TL
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS

import Identifiers as Idents

import Pr
import Web.Browser
import CSS.Internal


-- * DSL setup

declareFields [d|
  data Conf = Conf
    { confSelector :: Selector
    , confBrowser :: Browser
    }
  |]

declareFields [d|
  data State = State
    { stateIdents :: [TL.Text]
    , stateAnimationIdents :: [TL.Text]
    }
  |]

instance Default State where
  def = State idents' animationIdents'
    where
      idents' = map TL.fromStrict Idents.identifierSource
      animationIdents' = Idents.identifiersFilter forbidden <&> TL.fromStrict
      forbidden = ["none", "unset", "initial", "inherit"]
      -- ^ As by https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name

declareFields [d|
  data CSSW = CSSW
    { cSSWRules :: [Rule]
    , cSSWDecls :: [Declaration]
    }
  |]

instance Monoid CSSW where
  mempty = CSSW mempty mempty
  mappend a b = CSSW (a^.rules <> b^.rules) (a^.decls <> b^.decls)

type DM = Writer [Declaration]

type CSSM = RWST Conf CSSW State Identity
type M = CSSM

runCSSM :: Conf -> State -> CSSM () -> ([Rule], State)
runCSSM r s m = (r' : cssw^.rules, state)
  where
    (ret, state, cssw) = runRWS m r s
    r' = mkRule (r^.selector) (cssw^.decls)

-- * For export

run :: SelectorFrom a => Browser -> a -> CSSM () -> [Rule]
run b s m = runCSSM (Conf (selFrom s) b) def m & fst

rule :: SelectorFrom a => a -> DM () -> CSSM ()
rule s ds = tellRules $ pure $ mkRule (selFrom s) (execWriter ds)

prop
  :: (HasDecls w [Declaration], MonadWriter w m)
  => TL.Text -> Value -> m ()
prop k v = tellDecls $ pure $ mkDeclaration k v

tellRules rs = tell $ mempty & rules .~ rs
tellRule =  tellRules . pure

tellDecls ds = tell $ mempty & decls .~ ds

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
  tellRules' (Conf hoovered $ conf^.browser) m
  where
    pseudo' :: TL.Text -> SimpleSelector -> SimpleSelector
    pseudo' t s = s & pseudos %~ (Pseudo t:)

combinator :: SimpleSelectorFrom a => SOp -> a -> CSSM () -> CSSM ()
combinator c d m = let
  ss = ssFrom d
  in do
  conf <- ask
  let r = Conf (Combined c (conf^.selector) ss) (conf^.browser)
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
  keyframes' name km

keyframes' :: TL.Text -> KM () -> CSSM Value
keyframes' name km = do
  ks <- asks (view browser) <&> runReader (execWriterT km)
  tellRule $ Keyframes name ks
  return $ Word name

-- * At-rules

atRule :: TL.Text -> TL.Text -> CSSM () -> CSSM ()
atRule name e dm = do
  conf <- ask
  state0 <- get
  let (rules, state1) = runCSSM conf state0 dm
  tellRule $ AtRule name e rules
  put state1

media :: TL.Text -> CSSM () -> CSSM ()
media = atRule "media"

supports :: TL.Text -> CSSM () -> CSSM ()
supports = atRule "supports"
