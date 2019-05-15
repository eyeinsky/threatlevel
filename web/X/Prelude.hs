module X.Prelude
  ( module Export
  ) where

import Prelude2 as Export hiding
  ( div, rem, (/) -- Prelude
  , (.=), text, (?=) -- lens
  , on -- Data.Function
  , traceShowId -- Debug.Trace
  )

import Data.Default as Export
import GHC.Generics as Export (Generic)
import Control.Monad.IO.Class as Export
import Control.Monad.Reader as Export
import Debug.Trace as Export
