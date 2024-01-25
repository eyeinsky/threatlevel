module CSS.TH.Generated where

import Common.Prelude as P
import CSS.TH.Internal

-- TH-generate all properties with @prop@
concat <$> mapM declareCssProperty allProperties

-- TH-generate all pseudo-classes
$(
  let leaveOut = (`elem` ["left", "right"]) -- ^ These are also CSS properties
      predicate = not . either leaveOut leaveOut
  in concat <$> mapM declarePseudoClass (P.filter predicate pseudoClasses)
 )
-- TH-generate all pseoudo-elements
concat <$> mapM declarePseudoElement pseudoElements
