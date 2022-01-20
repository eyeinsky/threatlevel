{-# LANGUAGE AllowAmbiguousTypes #-}
module CSS.DSL.Polysemy.Effect where

import Common.Prelude
import qualified Data.Text as TS

import Polysemy
import Polysemy.Internal
import CSS.Syntax
import CSS.DSL.Common

data CSS s :: Effect where
  GetFreshClass :: CSS s m Class
  EmitRules :: Selector -> Declarations -> CSS s m ()
  EmitDeclarations :: Declarations -> CSS s m ()
  WithDerivedSelector
    :: (Selector -> Selector) -> Sem (CSS s : s) _a -> CSS s m ()

makeSem ''CSS
