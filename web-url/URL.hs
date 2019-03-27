module URL
  ( module Export
  , localhost, (?=)
  ) where

import qualified Data.Text as TS
import Prelude
import Control.Lens hiding ((?=))

import URL.Core as Export
import URL.Parse as Export

localhost :: URL
localhost = URL (Proto "http") auth (Path []) (Params []) (Fragment "")
  where
    auth = Authority Nothing (Domain "localhost") 80

-- * Shorthand to set url

instance Semigroup Params where
  Params a <> Params b = Params (a <> b)
instance Monoid Params where
  mempty = Params mempty
  mappend = (<>)

(?=) :: TS.Text -> TS.Text -> URL -> URL
(?=) k v url = url & params <>~ Params [(k, Just v)]
  where
    pair = k <> "=" <> v :: TS.Text
