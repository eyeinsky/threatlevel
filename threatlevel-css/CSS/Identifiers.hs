module CSS.Identifiers where

import Common.Prelude
import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Identifiers

identifiers :: Infinite TS.Text
identifiers = fmap TS.pack $ orderedFilter forbidden $ bigEndian ['a' .. 'z']
  where
    forbidden = ["none", "unset", "initial", "inherit"]
    -- ^ As by https://developer.mozilla.org/en-US/docs/Web/CSS/animation-name

identifiersLazy :: Infinite TL.Text
identifiersLazy = fmap TL.fromStrict identifiers
