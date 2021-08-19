module CSS.Monad where

import Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Control.Monad.RWS
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Reader

import Identifiers as Idents

import CSS.Syntax

-- * DSL setup

type Conf = Selector
type State = Infinite TL.Text

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

type CSSM = ReaderT Conf (WriterT CSSW Identity)
type M = CSSM

-- | Helper type alias
type Declarations = forall m w. (HasDecls w [Declaration], MonadWriter w m) => m ()

-- | Full runner for nested CSS
runCSSM :: Conf -> CSSM () -> [Rule]
runCSSM r m = (rule : cssw^.rules)
  where
    ((), cssw) = runWriter $ runReaderT m r
    rule = mkRule r (cssw^.decls)

rulesFor :: SelectorFrom s => s -> CSSM () -> [Rule]
rulesFor selectorLike m = runCSSM (selFrom selectorLike) m

-- * For export

rule :: SelectorFrom a => a -> DM () -> CSSM ()
rule s ds = tellRules $ pure $ mkRule (selFrom s) (execWriter ds)

prop :: TS.Text -> Value -> Declarations
prop k v = tellDecls $ pure $ Declaration k v


tellRules :: [Rule] -> CSSM ()
tellRules rs = tell $ mempty & rules .~ rs

tellRule :: Rule -> CSSM ()
tellRule =  tellRules . pure

tellDecls :: [Declaration] -> Declarations
tellDecls ds = tell $ mempty & decls .~ ds

apply :: (SimpleSelector -> SimpleSelector) -> Selector -> Selector
apply f s = go s
  where
    go :: Selector -> Selector
    go s = case s of
      Simple ss -> Simple (f ss)
      Combined op s ss -> Combined op s (f ss)

-- * Pseudo-class and -element

pseudoClassPlain :: TS.Text -> CSSM () -> CSSM ()
pseudoClassPlain name = pseudo' (pseudoPlain PseudoClass name)

pseudoClassArgumented :: TS.Text -> TS.Text -> CSSM () -> CSSM ()
pseudoClassArgumented name arg = pseudo' (pseudoArgd PseudoClass name arg)

pseudoElementPlain :: TS.Text -> CSSM () -> CSSM ()
pseudoElementPlain name = pseudo' (pseudoPlain PseudoElement name)

pseudoElementArgumented :: TS.Text -> TS.Text -> CSSM () -> CSSM ()
pseudoElementArgumented name arg = pseudo' (pseudoArgd PseudoElement name arg)

pseudo' :: (SimpleSelector -> SimpleSelector) -> CSSM () -> CSSM ()
pseudo' f m = do
  selector <- ask
  tellRules' (apply f selector) m :: CSSM ()

pseudo :: TS.Text -> CSSM () -> CSSM ()
pseudo = pseudoClassPlain
{-# DEPRECATED pseudo "Use the TH-generated classes instead." #-}

pseudoPlain
  :: (TS.Text -> Maybe TS.Text -> Pseudo) -> TS.Text
  -> SimpleSelector -> SimpleSelector
pseudoPlain dc t s = s & pseudos %~ (dc t Nothing:)

pseudoArgd
  :: (TS.Text -> Maybe TS.Text -> Pseudo) -> TS.Text -> TS.Text
  -> SimpleSelector -> SimpleSelector
pseudoArgd dc t a s = s & pseudos %~ (dc t (Just a):)

combinator :: SimpleSelectorFrom a => SOp -> a -> CSSM () -> CSSM ()
combinator op d m = let
  ss = ssFrom d
  in do
  currentSelector <- ask
  tellRules' (Combined op currentSelector ss) m

-- | Tell rules and thread state
tellRules' :: Conf -> CSSM () -> CSSM ()
tellRules' r m = tellRules $ runCSSM r m

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

-- * Keyframes

type KM = Writer [KeyframeBlock]

keyframe :: Double -> DeclM () -> KM ()
keyframe n dm = tell $ pure $ KeyframeBlock (KPercent n) (execWriter dm^.decls)

keyframes' :: TL.Text -> KM () -> (Value, Rule)
keyframes' name km = (Word name, keyframesRule)
  where keyframesRule = Keyframes name $ execWriter km

-- * At-rules

atRule :: TL.Text -> TL.Text -> CSSM () -> CSSM ()
atRule name e dm = do
  conf <- ask
  tellRule $ AtRule name e $ runCSSM conf dm

media :: TL.Text -> CSSM () -> CSSM ()
media = atRule "media"

supports :: TL.Text -> CSSM () -> CSSM ()
supports = atRule "supports"
