module Common.Lens where

import Control.Lens

class HasClasses s a | s -> a where
  classes :: Lens' s a
  {-# MINIMAL classes #-}
