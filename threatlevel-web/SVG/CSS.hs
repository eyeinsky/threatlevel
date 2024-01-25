module SVG.CSS where

import Prelude
import Data.List ((\\))
import SVG.TH (svg11presentationalAttributes)
import CSS.TH.Internal (declareCssProperty, allProperties)

-- * Presentational attributes
--
-- $mustUseNamedChunk
-- All presentational attributes can be used as CSS properties, so TH-generate all of them. See
-- <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/Presentation>

-- ** Properties

fmap concat $ mapM declareCssProperty (svg11presentationalAttributes \\ allProperties)
