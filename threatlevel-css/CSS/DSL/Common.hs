module CSS.DSL.Common where

import CSS.Syntax

apply :: (SimpleSelector -> SimpleSelector) -> Selector -> Selector
apply f s = go s
  where
    go :: Selector -> Selector
    go s = case s of
      Simple ss -> Simple (f ss)
      Combined op s ss -> Combined op s (f ss)
