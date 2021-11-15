module Common.TH where

import Prelude
import Data.String
import Language.Haskell.TH

-- | Add suffix to haskell reserved words
reserved_ :: (Eq a, IsString a, Semigroup a) => a -> a
reserved_ t = if t `elem` ["class", "type", "in", "default", "where"]
  then t <> "_"
  else t

declareFn :: Name -> TypeQ -> ExpQ -> DecsQ
declareFn name sig exp = (:) <$> name `sigD` [t| $sig |] <*> [d| $(varP name) = $exp |]
