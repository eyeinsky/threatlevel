module CSS.DSL.MTL where

import Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Control.Monad.RWS
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Reader

import Identifiers as Idents

import CSS.Syntax
import CSS.DSL.Common

-- * DSL setup

type Conf = Selector
type Names = Infinite TL.Text

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

type CSSM = ReaderT Conf (WriterT CSSW Identity) ()
type M = CSSM

-- | Helper type alias
type DeclarationsM = forall m w. (HasDecls w [Declaration], MonadWriter w m) => m ()

-- | Full runner for nested CSS
runCSSM :: Conf -> CSSM -> [Rule]
runCSSM r m = (rule : cssw^.rules)
  where
    ((), cssw) = runWriter $ runReaderT m r
    rule = mkRule r (cssw^.decls)

rulesFor :: SelectorFrom s => s -> CSSM -> [Rule]
rulesFor selectorLike m = runCSSM (selFrom selectorLike) m

-- * For export

prop :: TS.Text -> Value -> DeclarationsM
prop k v = tell $ mempty & decls .~ (pure $ Declaration k v)

tellRules :: [Rule] -> CSSM
tellRules rs = tell $ mempty & rules .~ rs

-- * Pseudo-class and -element

pseudo :: (SimpleSelector -> SimpleSelector) -> CSSM () -> CSSM ()
pseudo f m = do
  selector <- ask
  tellRules' (apply f selector) m :: CSSM ()

combinator :: SimpleSelectorFrom a => SOp -> a -> CSSM -> CSSM
combinator op d m = let
  ss = ssFrom d
  in do
  currentSelector <- ask
  tellRules' (Combined op currentSelector ss) m

-- | Tell rules and thread state
tellRules' :: Conf -> CSSM -> CSSM
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

keyframes' :: TS.Text -> KM () -> (Value, Rule)
keyframes' name km = (Word name, keyframesRule)
  where keyframesRule = Keyframes name $ execWriter km

-- * At-rules

atRule :: TS.Text -> TS.Text -> CSSM -> CSSM
atRule name e dm = do
  conf <- ask
  tellRules $ pure $ AtRule name e $ runCSSM conf dm

media :: TS.Text -> CSSM () -> CSSM ()
media = atRule "media"

supports :: TS.Text -> CSSM () -> CSSM ()
supports = atRule "supports"
