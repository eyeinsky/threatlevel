{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE UndecidableInstances #-}
module CSS.Internal where

import Pr
import Data.Text.Format

import Numeric (showHex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Word
import qualified Data.DList as D

import Control.Monad.Writer
import Control.Monad.Identity

import DOM.Core hiding (Value)
import Render (renderM, Render, (<+>))
import qualified Render as R

-- * Syntax

-- ** Declaration

data Property = Property TL.Text
instance Render Property where renderM (Property a) = pure a

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

   | Url TL.Text

   | Compound (D.DList Value)

instance Semigroup Value where
  Compound xs <> Compound ys = Compound (xs <> ys)
  Compound xs <> v = Compound (xs <> pure v)
  v <> Compound xs = Compound (pure v <> xs)
  x <> y = Compound (pure x <> pure y)
instance Monoid Value where
  mempty = error "CSS.Internal: Value can't be empty"

prc i = Percent i
px i = Px i
pt i = Points i
em i = Em i
rem i = Rem i
vh i   = ViewportHeight i
vw i   = ViewportWidth  i
vmin i = ViewportMin  i
vmax i = ViewportMax  i

hex a     = ColorHex a
rgb a b c = ColorRGB a b c
rgba a b c d = ColorRGBA a b c d

str = Word

instance Render Value where
   renderM a = case a of
      Word a -> pure a
      String a -> renderM (Comment "long strings unimplemented")

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

data Pseudo = Pseudo TL.Text deriving (Eq)
instance Render Pseudo where renderM (Pseudo a) = pure $ ":" <> a
instance Render TagName where renderM (TagName a) = renderM a
instance Render Id     where renderM (Id     a) = ("#" <>) <$> renderM a
instance Render Class  where renderM (Class  a) = ("." <>) <$> renderM a

declareFields [d|
  -- | Element, class, id or pseudo
  data SimpleSelector = SimpleSelector
    { simpleSelectorTag :: Maybe TagName
    , simpleSelectorMaybeId :: Maybe Id
    , simpleSelectorClasses :: [Class]
    , simpleSelectorPseudos :: [Pseudo]
    }
  |]

instance Render SimpleSelector where
   renderM (SimpleSelector mt mi cs ps)
     = g mt <+> g mi <+> (TL.concat <$> (f cs <+> f ps))
      where f = mapM renderM
            g = maybe (pure "") renderM


data Declaration = Declaration Property Value
instance Render Declaration where
   renderM (Declaration p v) = R.mseq [renderM p, pure ":", renderM v]
instance Render [Declaration] where
   renderM ds = TL.concat . map (<> ";") <$> mapM renderM ds

-- ** Selector

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

data KeyframeSelector = From | To | KPercent Int
instance Render KeyframeSelector where
   renderM ks = pure $ case ks of
     From -> "from"
     To -> "to"
     KPercent i -> R.tshow i <> "%"

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
         Keyframes name blocks -> False
         AtRule _ _ rs -> null rs

data Rule where
  Qualified :: Prelude -> [Declaration] -> Rule
  Keyframes :: TL.Text -> [KeyframeBlock] -> Rule
  AtRule :: TL.Text -> TL.Text -> CSS -> Rule
instance Render Rule where
   renderM r = case r of
      Qualified p ds -> renderM p <+> (R.curly <$> (renderM ds))
      Keyframes name blocks -> pure "@keyframes " <+> pure name <+> blocks'
        where
          blocks' = R.curly . TL.concat <$> mapM renderM blocks
      AtRule name tl rs -> let
        query = "@" <> name <> " " <> tl
        in pure query <+> (R.curly . TL.concat <$> mapM renderM rs)

data Prelude = Selectors [Selector]
instance Render Prelude where
   renderM (Selectors ss) = TL.intercalate "," <$> mapM renderM ss

-- ** Helpers

mkRule :: Selector -> [Declaration] -> Rule
mkRule s ds = Qualified (Selectors [s]) ds

mkDeclaration :: TL.Text -> Value -> Declaration
mkDeclaration p v = Declaration (Property p) v

-- ** Instances

deriving instance Show Rule
deriving instance Show KeyframeBlock
deriving instance Show KeyframeSelector
deriving instance Show Prelude
deriving instance Show Selector
deriving instance Show SOp
deriving instance Show SimpleSelector
deriving instance Show Declaration
deriving instance Show Property
deriving instance Show Value
deriving instance Show Comment

deriving instance Show Pseudo

-- * Convenience

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
instance SimpleSelectorFrom TagName where
  ssFrom a = SimpleSelector (Just a) Nothing [] []
instance SimpleSelectorFrom [Class] where
  ssFrom a = SimpleSelector Nothing Nothing a []
instance SimpleSelectorFrom Class where
  ssFrom a = SimpleSelector Nothing Nothing [a] []
instance SimpleSelectorFrom Id where
  ssFrom a = SimpleSelector Nothing (Just a) [] []
instance SimpleSelectorFrom Pseudo where
  ssFrom a = SimpleSelector Nothing Nothing [] [a]


instance IsString Class where
  fromString = Class . fromString

instance IsString Value where
  fromString = str . TL.pack

instance IsString SimpleSelector where
  fromString s = case s of
    '#' : rest -> fromId rest
    '.' : rest -> fromClass rest
    ':' : rest -> fromPseudo rest
    _ -> fromTag s
    where
      fromId s = SimpleSelector Nothing (Just $ Id $ fromString s) [] []
      fromClass s = SimpleSelector Nothing Nothing [Class $ fromString s] []
      fromPseudo s = SimpleSelector Nothing Nothing [] [Pseudo $ TL.pack s]
      fromTag s = SimpleSelector (Just $ fromString s) Nothing [] []

instance IsString TagName where
  fromString = TagName . fromString

instance Num Value where
  fromInteger = Int . fromInteger
