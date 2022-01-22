module CSS.DSL.MTL
  ( module CSS.DSL.MTL
  , module CSS.DSL.Common
  ) where

import Prelude
import qualified Data.Text as TS
import Control.Monad.RWS
import Control.Lens
import Control.Monad.Writer
import Control.Monad.Reader

import Render

import CSS.Syntax
import CSS.DSL.Common

-- * DSL setup

declareFields [d|
  data CSSW = CSSW
    { cSSWRules :: [OuterRule]
    , cSSWDecls :: [Declaration]
    }
  |]

instance Semigroup CSSW where
  a <> b = CSSW (a^.rules <> b^.rules) (a^.decls <> b^.decls)
instance Monoid CSSW where
  mempty = CSSW mempty mempty

instance HasDecls [Declaration] [Declaration] where
  decls = id

type CSSM = ReaderT Selector (WriterT CSSW Identity) ()
type M = CSSM

-- | Helper type alias
type PolyProp = forall m w. (HasDecls w [Declaration], MonadWriter w m) => m ()

-- | Full runner for nested CSS
runCSSM :: Selector -> CSSM -> [OuterRule]
runCSSM r m = (rule : cssw^.rules)
  where
    ((), cssw) = runWriter $ runReaderT m r
    rule = mkRule r (cssw^.decls)

rulesFor :: SelectorFrom s => s -> CSSM -> [OuterRule]
rulesFor selectorLike m = runCSSM (selFrom selectorLike) m

-- * For export

prop :: TS.Text -> Value -> PolyProp
prop k v = tell $ mempty & decls .~ (pure $ Declaration k v)

tellRules :: [OuterRule] -> CSSM
tellRules rs = tell $ mempty & rules .~ rs

-- * Pseudo-class and -element

combinator :: SimpleSelectorFrom a => SOp -> a -> CSSM -> CSSM
combinator op d m = let
  ss = ssFrom d
  in do
  currentSelector <- ask
  tellRules' (Combined op currentSelector ss) m

-- | Tell rules and thread state
tellRules' :: Selector -> CSSM -> CSSM
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

type MonoProp = Writer DeclW
instance Render (MonoProp a) where
  type Conf (MonoProp a) = CSS.Syntax.Conf
  renderM m = renderM $ view decls $ execWriter m

-- * Keyframes

type KM = Writer [KeyframeBlock]

keyframe :: Double -> MonoProp () -> KM ()
keyframe n dm = tell $ pure $ KeyframeBlock (KPercent n) (execWriter dm^.decls)

keyframes' :: TS.Text -> KM () -> (Value, OuterRule)
keyframes' name km = (Word name, keyframesRule)
  where keyframesRule = Keyframes name $ execWriter km

-- * At-rules

atRule :: TS.Text -> TS.Text -> CSSM -> CSSM
atRule name e dm = do
  conf <- ask
  tellRules $ pure $ AtRule name e $ runCSSM conf dm

-- * Compat (CSS.TH, Web.*)

type CSSF = CSSM -> CSSM

combine :: (Selector -> Selector) -> CSSF
combine mod m = do
  selector <- ask
  tellRules' (mod selector) m :: CSSM
