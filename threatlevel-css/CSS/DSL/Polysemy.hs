module CSS.DSL.Polysemy
  ( module CSS.DSL.Polysemy
  , module Export
  ) where

import Common.Prelude
import Polysemy
import qualified Data.Text as TS

import CSS.Syntax
import CSS.DSL.Polysemy.Effect as Export
import CSS.DSL.Polysemy.Base as Export


class_ :: Member CSS r => Declarations -> Sem r Class
class_ ds = do
  c <- getFreshClass
  emitRules (selFrom c) ds
  return c
