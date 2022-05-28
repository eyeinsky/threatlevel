module XML
  ( module XML
  , module Export
  ) where

import Common.Prelude
import Data.Text.Lazy.Lens hiding (text)
import XML.Core as Export hiding ((!-))
import XML.Render as Export

showHtml
  :: (Profunctor p, Contravariant f, Show a1, Functor f)
  => p (XMLM ns a) (f (XMLM ns a)) -> p a1 (f a1)
showHtml = showXml

showXml
  :: (Profunctor p, Contravariant f, Show a1, Functor f)
  => p (XMLM ns a) (f (XMLM ns a)) -> p a1 (f a1)
showXml = to show.packed.to text
