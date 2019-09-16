module SVG.Examples where

import X.Prelude
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as HM

import HTML hiding (svg)
import SVG
import Render

e = svg ! cls_ [Class "myClass"]
        ! cls_ [Class "myOtherClass"]
        ! id_  (Id "myId")
        ! id_  (Id "myLastId")
        $ do
  rect $ "jee"
