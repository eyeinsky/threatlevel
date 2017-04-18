module Web.HTML
  ( module Web.HTML
  , module Web.HTML.Core
  , module Web.HTML.Render
  , module Web.HTML.Shorthands
  ) where

import Web.HTML.Core hiding ((!))
import qualified Web.HTML.Core as W
import Web.HTML.Render
import Web.HTML.Shorthands

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
