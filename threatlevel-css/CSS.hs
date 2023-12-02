module CSS
  ( module CSS
  , module CSS.Syntax
  , module CSS.DSL
  , module CSS.TH
--  , keyframes', keyframe
  ) where

import Common.Prelude as P
import Data.Text qualified as TS
import CSS.Syntax hiding
  ( tag, maybeId, pseudos
  )
import CSS.DSL
import CSS.TH

-- | TH-generate all properties with @prop@
concat <$> mapM declareCssProperty allProperties

-- | TH-generate all pseudo-classes
$(
  let leaveOut = (`elem` ["left", "right"]) -- ^ These are also CSS properties
      predicate = not . either leaveOut leaveOut
  in concat <$> mapM declarePseudoClass (P.filter predicate pseudoClasses)
 )
-- | TH-generate all pseoudo-elements
concat <$> mapM declarePseudoElement pseudoElements

descendant, child, sibling, generalSibling :: forall m a . (CSS m, SimpleSelectorFrom a) => a -> m () -> m ()
descendant = combinator Descendant
child = combinator Child
sibling = combinator Sibling
generalSibling = combinator GeneralSibling

tagSelector :: TS.Text -> Tag
tagSelector = Tag

-- * Useful styles

centerContent :: Prop m => m ()
centerContent = do
  display "flex"
  flexFlow "column nowrap"
  justifyContent "center"
  alignItems "center"

flexbox :: Prop m => Value -> m ()
flexbox how = do
  display "flex"
  flexFlow how

square :: Prop m => Value -> m ()
square n = do
  width n
  height n

circle :: Prop m => Value -> m ()
circle n = do
  square n
  borderRadius $ prc 50
