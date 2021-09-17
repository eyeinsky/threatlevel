module URL
  ( module Export
  , localhost
  ) where

import Prelude

import URL.Core as Export
import URL.Parse as Export
import URL.Render as Export
import URL.TH as Export
import Render as Export (render')

localhost :: URL
localhost = URL (Proto "http") auth (Path []) (Params []) (Fragment "")
  where
    auth = Authority Nothing (Domain "localhost") 80
