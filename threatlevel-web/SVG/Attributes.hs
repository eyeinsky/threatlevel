{-# OPTIONS_GHC -Wno-missing-signatures #-}
module SVG.Attributes where

import X.Prelude
import XML
import XML.TH
import SVG.TH

concat <$> mapM (mkAttr 'Custom [t|Attribute|]) svg11allAttributes
