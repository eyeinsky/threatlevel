module URL
  ( module Export
  , localhost, (?=)
  ) where

import qualified Data.Text as TS
import Prelude
import Control.Lens hiding ((?=))

import URL.Core as Export
import URL.Parse as Export
import URL.Render as Export
import Render as Export (render')

localhost :: URL
localhost = URL (Proto "http") auth (Path []) (Params []) (Fragment "")
  where
    auth = Authority Nothing (Domain "localhost") 80

-- * Shorthand to set url

(?=) :: TS.Text -> TS.Text -> URL -> URL
(?=) k v url = url & params <>~ Params [(k, Just v)]
