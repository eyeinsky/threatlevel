module CSS.Identifiers where

import Prelude
import Data.Text
import qualified Data.Text.Lazy as TL
import Identifiers

identifiers :: Infinite Text
identifiers = fmap pack $ orderedFilter forbidden $ bigEndian ['a' .. 'z']
  where
    forbidden = ["none", "unset", "initial", "inherit"]
    -- ^ As by https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name

identifiersLazy :: Infinite TL.Text
identifiersLazy = fmap TL.fromStrict identifiers
