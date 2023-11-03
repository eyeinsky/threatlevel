module XML
  ( module XML
  , module Export
  ) where

import X.Prelude
import XML.Core as Export hiding ((!-))
import XML.Render as Export

showHtml :: forall k (p :: * -> * -> *) (f :: * -> *) a1 (ns :: k)
                         a2 (c :: * -> Constraint).
                  (Profunctor p, Contravariant f, Show a1, Functor f) =>
                  p (Writer [XML ns a2 c] ()) (f (Writer [XML ns a2 c] ()))
                  -> p a1 (f a1)
showHtml = showXml

showXml :: forall k (p :: * -> * -> *) (f :: * -> *) a1 (ns :: k)
                  a2 (c :: * -> Constraint).
           (Profunctor p, Contravariant f, Show a1, Functor f) =>
           p (Writer [XML ns a2 c] ()) (f (Writer [XML ns a2 c] ()))
           -> p a1 (f a1)
showXml = to show.packed.to text
