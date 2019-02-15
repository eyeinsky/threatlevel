module XML
  ( module XML
  , module Export
  ) where

import XML.Core as Export hiding ((!-))
import XML.Render as Export

import Pr

-- showHtml :: Show s => s -> XML ns a c
showHtml = showXml

showXml = to show.packed.to text
