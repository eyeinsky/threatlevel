module HTML
  ( module HTML
  , module HTML.Core
  , module HTML.Render
  , module HTML.Shorthands
  ) where

import Pr
import Data.Text.Lazy.Lens (utf8)
import HTML.Core hiding ((!))
import qualified HTML.Core as W
import HTML.Render
import HTML.Shorthands

-- *

renderRaw :: forall a. Render a => a -> W.Html
renderRaw x = W.text (render x)


-- * Exclamatable

class Exclamatable e a where
  (!) :: e -> a -> e

instance Exclamatable (HTMLM ()) W.Id where
  (!) e id = e W.! id_ id
instance Exclamatable (HTMLM () -> HTMLM ()) W.Id where
  (!) e id = e W.! id_ id

instance Exclamatable (HTMLM ()) [W.Class] where
  (!) e cs = e W.! cls_ cs
instance Exclamatable (HTMLM () -> HTMLM ()) [W.Class] where
  (!) e cs = e W.! cls_ cs

instance Exclamatable (HTMLM ()) W.Class where
  (!) e c = e W.! cls_ [c]
instance Exclamatable (HTMLM () -> HTMLM ()) W.Class where
  (!) e c = e W.! cls_ [c]

instance Exclamatable (HTMLM ()) W.Attribute where
  (!) e c = e W.! c
instance Exclamatable (HTMLM () -> HTMLM ()) W.Attribute where
  (!) e c = e W.! c
