module Common.TH where

import Prelude
import Data.String

-- | Add suffix to haskell reserved words
reserved_ :: (Eq a, IsString a, Semigroup a) => a -> a
reserved_ t = if t `elem` ["class", "type", "in", "default", "where"]
  then t <> "_"
  else t
