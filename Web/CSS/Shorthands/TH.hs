module Web.CSS.Shorthands.TH where

import Prelude
import Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH

shorthand :: String -> DecsQ
shorthand propName = [d| $(varP $ mkName name') = prop $(stringE propName) |]
  where
    x : xs = TL.splitOn "-" $ TL.pack propName
    f t = let (a, b) = TL.splitAt 1 t
      in TL.toUpper a <> b
    name' = TL.unpack $ TL.concat $ x : map f xs
