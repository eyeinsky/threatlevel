module XML
  ( module XML
  , module Export
  ) where

import X.Prelude
import XML.Core as Export hiding ((!-))
import XML.Render as Export
import Data.Kind

showHtml = showXml

showXml = to show.packed.to text
