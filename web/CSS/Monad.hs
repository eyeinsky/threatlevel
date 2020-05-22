module CSS.Monad where

import qualified Data.Text.Lazy as TL
import Control.Monad.RWS

import Identifiers as Idents

import X.Prelude hiding (State)

import CSS.Syntax

-- * DSL setup

declareFields [d|
  data Conf = Conf
    { confSelector :: Selector
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

instance Semigroup CSSW where
  a <> b = CSSW (a^.rules <> b^.rules) (a^.decls <> b^.decls)
instance Monoid CSSW where
  mempty = CSSW mempty mempty

instance HasDecls [Declaration] [Declaration] where
  decls = id

type DM = Writer [Declaration]

type CSSM = RWST Conf CSSW State Identity
type M = CSSM

-- | Full runner for nested CSS
runCSSM :: Conf -> State -> CSSM () -> ([Rule], State)
runCSSM r s m = (r' : cssw^.rules, state)
  where
    (_, state, cssw) = runRWS m r s
    r' = mkRule (r^.selector) (cssw^.decls)

-- * For export

run :: SelectorFrom a => a -> CSSM () -> [Rule]
run s m = runCSSM (Conf (selFrom s)) def m & fst

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
  tellRules' (Conf hoovered) m
  where
    pseudo' :: TL.Text -> SimpleSelector -> SimpleSelector
    pseudo' t s = s & pseudos %~ (Pseudo t:)

combinator :: SimpleSelectorFrom a => SOp -> a -> CSSM () -> CSSM ()
combinator c d m = let
  ss = ssFrom d
  in do
  conf <- ask
  let r = Conf (Combined c (conf^.selector) ss)
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

instance Semigroup DeclW where
  a <> b = DeclW (a^.decls <> b^.decls)
instance Monoid DeclW where
  mempty = DeclW mempty

type DeclM = Writer DeclW
execDeclM :: DeclM a ->  DeclW
execDeclM dm = execWriter dm

type KM = Writer [KeyframeBlock]

keyframe :: Double -> DeclM () -> KM ()
keyframe n dm = tell $ pure $ KeyframeBlock (KPercent n) (execWriter dm^.decls)

keyframes :: KM () -> CSSM Value
keyframes km = do
  name <- Idents.next animationIdents
  keyframes' name km

keyframes' :: TL.Text -> KM () -> CSSM Value
keyframes' name km = do
  tellRule $ Keyframes name $ execWriter km
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
