module X.Prelude
  ( module Export
  ) where

import Prelude2 as Export hiding
  ( div, rem, (/) -- Prelude
  , (.=), text -- lens
  , on -- Data.Function
  )

import Data.Default as Export
