{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module CSS.Syntax where

import Prelude
import Data.String
import Data.Text.Format

import Numeric (showHex)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Word
import qualified Data.DList as D
import Text.Printf
import Control.Lens

import Common.Lens
import Render (renderM, Render, (<+>))
import qualified Render as R

-- * Syntax

-- ** Declaration

data Value
   = Word TL.Text
   | String TL.Text

   | Percent Double
   | Em Double
   | Rem Double
   | Px Int
   | Int Int
   | Points Double

   | ViewportHeight  Double
   | ViewportWidth   Double
   | ViewportMin     Double
   | ViewportMax     Double

   | Time Double

   | ColorHex Word32
   | ColorRGB Word8 Word8 Word8
   | ColorRGBA Word8 Word8 Word8 Double
   | ColorHSL Double Double Double
   | ColorHSLA Double Double Double Double

   | Url TL.Text

   | Compound (D.DList Value)

instance Semigroup Value where
  Compound xs <> Compound ys = Compound (xs <> ys)
  Compound xs <> v = Compound (xs <> pure v)
  v <> Compound xs = Compound (pure v <> xs)
  x <> y = Compound (pure x <> pure y)
instance Monoid Value where
  mempty = error "CSS.Internal: Value can't be empty"

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
vh i   = ViewportHeight i

vw :: Double -> Value
vw i   = ViewportWidth  i

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

str :: TL.Text -> Value
str = Word

instance Render Value where
   renderM a = case a of
      Word a -> pure a
      String _ -> renderM (Comment "long strings unimplemented")

      Percent a -> pure $ R.tshow a <> "%"
      Em a -> pure $ p a <> "em"
      Rem a -> pure $ p a <> "rem"
      Px a -> pure $ p a <> "px"
      Points a -> pure $ p a <> "pt"
      Int a -> pure $ p a
      Time a -> pure $ p a <> "s"

      ViewportWidth  a -> pure $ p a <> "vw"
      ViewportHeight a -> pure $ p a <> "vh"
      ViewportMin    a -> pure $ p a <> "vmin"
      ViewportMax    a -> pure $ p a <> "vmax"

      ColorHex w32 -> pure $ "#" <> hex w32
      ColorRGB a b c -> pure $ format "rgb({},{},{})" (a,b,c)
      ColorRGBA a b c d -> pure $ format "rgba({},{},{}, {})" (a,b,c,d)
      ColorHSL a b c -> pure $ format "hsl({},{}%,{}%, {})" (a,b,c)
      ColorHSLA a b c d -> pure $ format "hsla({},{}%,{}%, {})" (a,b,c,d)

      Url tl -> pure $ "url(\"" <> tl <> "\")"
      Compound l -> TL.intercalate " " <$> mapM renderM (D.toList l)
      where
        hex a = TL.pack $ showHex a ""
        p = R.tshow

-- ** Comment

data Comment = Comment TL.Text
instance Render Comment where
   renderM (Comment a) = pure $ R.sur "/*" "*/" a

-- ** Selector

-- *** Pseudo-classes and -elements

data Pseudo
  = PseudoClass TS.Text (Maybe TS.Text)
  | PseudoElement TS.Text (Maybe TS.Text)
  deriving (Eq)

instance Render Pseudo where
  renderM p = pure $ case p of
    PseudoClass a arg -> f ":" a arg
    PseudoElement a arg -> f "::" a arg
    where
      f prefix a arg = prefix <> TL.fromStrict a <> maybe "" mkArg arg
      mkArg a = "(" <> TL.fromStrict a <> ")"

-- *** Attirbute selector

data AttributeSelector
  = Has
  | Equals
  | WhiteSpaceContains
  | EqualsOrDashPrefix
  | Starts
  | Ends
  | Contains
  deriving (Eq, Show)

-- *** Simple selector

-- | Element, class, id or pseudo
data SimpleSelector = SimpleSelector
  { simpleSelectorTag :: Maybe TS.Text
  , simpleSelectorMaybeId :: Maybe TS.Text
  , simpleSelectorClasses :: [TS.Text]
  , simpleSelectorPseudos :: [Pseudo]
  , simpleSelectorAttributeSelectors :: [AttributeSelector]
  }
makeFields ''SimpleSelector

instance Render SimpleSelector where
   renderM (SimpleSelector maybeTag maybeId cs ps _)
     = pure (maybePrefix "" maybeTag <> maybePrefix "#" maybeId)
     <+> (TL.concat . (map mkClass cs <>) <$> mapM renderM ps)
     where
       maybePrefix :: TL.Text -> Maybe TS.Text -> TL.Text
       maybePrefix p m = maybe "" ((p <>) . TL.fromStrict) m
       mkClass :: TS.Text -> TL.Text
       mkClass t = TL.fromStrict $ "." <> t

data Declaration = Declaration TS.Text Value
instance Render Declaration where
   renderM (Declaration p v) = R.mseq [pure $ TL.fromStrict p, pure ":", renderM v]
instance Render [Declaration] where
   renderM ds = TL.concat . map (<> ";") <$> mapM renderM ds

-- *** Selector

data Selector where
  Simple :: SimpleSelector -> Selector
  Combined :: SOp -> Selector -> SimpleSelector -> Selector

data SOp = Descendant | Child | Sibling | GeneralSibling
instance Render SOp where
  renderM s = pure $ case s of
    Descendant -> " "
    Child -> ">"
    Sibling -> "+"
    GeneralSibling -> "~"

instance Render Selector where
  renderM s = case s of
    Simple ss -> renderM ss
    Combined op s s' -> renderM s <+> renderM op <+> renderM s'

-- ** Keyframe

data KeyframeSelector = From | To | KPercent Double
instance Render KeyframeSelector where
   renderM ks = pure $ case ks of
     From -> "from"
     To -> "to"
     KPercent d -> TL.pack (printf "%.4f" d) <> "%"

data KeyframeBlock
  = KeyframeBlock KeyframeSelector [Declaration]
instance Render KeyframeBlock where
  renderM (KeyframeBlock s ds) = renderM s <+> (R.curly <$> renderM ds)

-- ** Rule

type CSS = [Rule]
instance Render CSS where
   renderM li = TL.unlines <$> mapM renderM (filter (not . isEmpty) li)
     where
       isEmpty r = case r of
         Qualified _ ds -> null ds
         Keyframes _ _ -> False
         AtRule _ _ rs -> null rs
         FontFace rs -> null rs

data Rule where
  Qualified :: Prelude -> [Declaration] -> Rule
  Keyframes :: TL.Text -> [KeyframeBlock] -> Rule
  AtRule :: TL.Text -> TL.Text -> CSS -> Rule
  FontFace :: [Declaration] -> Rule
instance Render Rule where
   renderM r = case r of
      Qualified p ds -> renderM p <+> curlyRules ds
      Keyframes name blocks -> pure "@keyframes " <+> pure name <+> blocks'
        where
          blocks' = R.curly . TL.concat <$> mapM renderM blocks
      AtRule name tl rs -> let
        query = "@" <> name <> " " <> tl
        in pure query <+> (R.curly . TL.concat <$> mapM renderM rs)
      FontFace ds -> pure "@font-face " <+> curlyRules ds
      where
        curlyRules ds = R.curly <$> (renderM ds)

data Prelude = Selectors [Selector]
instance Render Prelude where
   renderM (Selectors ss) = TL.intercalate "," <$> mapM renderM ss

-- ** Helpers

mkRule :: Selector -> [Declaration] -> Rule
mkRule s ds = Qualified (Selectors [s]) ds

-- ** Instances

deriving instance Show Rule
deriving instance Show KeyframeBlock
deriving instance Show KeyframeSelector
deriving instance Show Prelude
deriving instance Show Selector
deriving instance Show SOp
deriving instance Show SimpleSelector
deriving instance Show Declaration
deriving instance Show Value
deriving instance Show Comment
deriving instance Show Pseudo

-- * Convenience

tagSelector :: TS.Text -> SimpleSelector
tagSelector t = SimpleSelector (Just t) Nothing [] [] []

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

instance IsString Value where
  fromString = str . TL.pack

instance IsString SimpleSelector where
  fromString s = case s of
    '#' : rest -> fromId rest
    '.' : rest -> fromClass rest
    ':' : rest -> fromPseudoClass rest
    _ -> fromTag s
    where
      fromId s = SimpleSelector Nothing (Just $ fromString s) [] [] []
      fromClass s = SimpleSelector Nothing Nothing [fromString s] [] []
      fromPseudoClass s = SimpleSelector Nothing Nothing [] [PseudoClass (TS.pack s) Nothing] []
      fromTag s = SimpleSelector (Just $ fromString s) Nothing [] [] []

instance Num Value where
  fromInteger = Int . fromInteger
