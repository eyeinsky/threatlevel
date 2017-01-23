module Web.SVG.Examples where

import Prelude2
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as HM

import Web.HTML hiding (svg)
import Web.SVG

e = svg ! cls_ [Class "myClass"]
        ! cls_ [Class "myOtherClass"]
        ! id_  (Id "myId")
        ! id_  (Id "myLastId")
        $ do
  rect $ "jee"

pr :: HTMLM () -> TL.Text
pr = TL.concat . map render . execWriter
