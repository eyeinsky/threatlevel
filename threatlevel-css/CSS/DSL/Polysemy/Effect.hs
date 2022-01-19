module CSS.DSL.Polysemy.Effect where

import Common.Prelude
import qualified Data.Text as TS

import Polysemy
import Polysemy.Internal
import CSS.Syntax
import CSS.DSL.Common

data CSS m a where
  GetFreshClass :: CSS m Class
  EmitRules :: Selector -> Declarations -> CSS m ()
  EmitDeclarations :: Declarations -> CSS m ()

makeSem ''CSS
