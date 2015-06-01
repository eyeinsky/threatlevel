module JS_Derive where

import Prelude2
import Data.Aeson
import Data.Aeson.TH
import Language.Haskell.TH

import JS

data X
   = A { x :: Int, y :: Prelude2.String }
   | B { z :: Int }
{-
toJSON/fromJSON

jsA x y = [("x", x), ("y", y), ("tag", "X")]
jsB z   = [("z", z)          , ("tag", "B")]
-}

deriveJS opts t = do
   ds <- deriveJSON opts t
   x <- u
   return (ds <> x)
   
mkFunc dataConstr = funD funcName []
   where
      funcName = "js" <> nameBase dataConstr

