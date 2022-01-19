module CSS.Syntax
  ( module CSS.Syntax
  , module CSS.Syntax.AST
  ) where

import Common.Prelude
import Data.Word
import qualified Data.Text as TS

import CSS.Syntax.AST
import CSS.Syntax.Render ()


-- | Conversion from something to Selector
class SelectorFrom a where selFrom :: a -> Selector
instance SelectorFrom Selector where
   selFrom = id
instance {-# OVERLAPPABLE #-} SimpleSelectorFrom a => SelectorFrom a where
   selFrom a = Simple (ssFrom a)

-- | Conversion from something to SimpleSelector
class SimpleSelectorFrom a where
  ssFrom :: a -> SimpleSelector
instance SimpleSelectorFrom SimpleSelector where
  ssFrom = id
instance SimpleSelectorFrom Pseudo where
  ssFrom a = SimpleSelector Nothing Nothing [] [a] []
instance SimpleSelectorFrom Tag where
  ssFrom a = SimpleSelector (Just a) Nothing [] [] []
instance SimpleSelectorFrom Id where
  ssFrom a = SimpleSelector Nothing (Just a) [] [] []
instance SimpleSelectorFrom Class where
  ssFrom a = SimpleSelector Nothing Nothing [a] [] []

-- * Unit shorthands

prc :: Double -> Value
prc i = Percent i

px :: Int -> Value
px i = Px i

pt :: Double -> Value
pt i = Points i

em :: Double -> Value
em i = Em i

rem :: Double -> Value
rem i = Rem i

vh :: Double -> Value
vh i = ViewportHeight i

vw :: Double -> Value
vw i = ViewportWidth  i

vmin :: Double -> Value
vmin i = ViewportMin  i

vmax :: Double -> Value
vmax i = ViewportMax  i

hex :: Word32 -> Value
hex a = ColorHex a

rgb :: Word8 -> Word8 -> Word8 -> Value
rgb a b c = ColorRGB a b c

rgba :: Word8 -> Word8 -> Word8 -> Double -> Value
rgba a b c d = ColorRGBA a b c d

hsl :: Double -> Double -> Double -> Value
hsl a b c = ColorHSL a b c

hsla :: Double -> Double -> Double -> Double -> Value
hsla a b c d = ColorHSLA a b c d

alpha :: Double -> Value
alpha a = rgba 0 0 0 a

str :: TS.Text -> Value
str = Word
