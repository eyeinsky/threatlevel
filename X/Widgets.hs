module X.Widgets where

import Prelude2 hiding (div)
import X

-- parrallax :: _
parrallaxContainer bgValue = do
  container <- styled div $ do
    display "block"
    backgroundImage bgValue
    backgroundAttachment "fixed"
    backgroundPosition "center"
    backgroundRepeat "no-repeat"
    -- @media all and (max-width: 778px) { background-position: right; }

  return container
