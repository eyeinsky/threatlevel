module SVG.CSS where

import Prelude
import Data.List ((\\))
import SVG.TH (svg11presentationalAttributes)
import CSS.TH (declareCssProperty, allProperties)

-- | All presentational attributes can be used as CSS properties, so TH-generate all of them.
fmap concat $ mapM declareCssProperty (svg11presentationalAttributes \\ allProperties)
