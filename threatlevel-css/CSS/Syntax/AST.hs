module CSS.Syntax.AST where

import Common.Prelude
import Data.Word
import qualified Data.DList as D
import qualified Data.Text as TS


data Value
  = Word TS.Text
  | String TS.Text

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

  | Url TS.Text
  | Compound (D.DList Value)
  | Important

instance Semigroup Value where
  Compound xs <> Compound ys = Compound (xs <> ys)
  Compound xs <> v = Compound (xs <> pure v)
  v <> Compound xs = Compound (pure v <> xs)
  x <> y = Compound (pure x <> pure y)

instance Monoid Value where
  mempty = error "CSS.Internal: Value can't be empty"

data Comment = Comment TS.Text

-- * Selector

data Tag = Tag TS.Text | Any
newtype Id = Id TS.Text
newtype Class = Class TS.Text

data Pseudo
  = PseudoClass TS.Text (Maybe TS.Text)
  | PseudoElement TS.Text (Maybe TS.Text)
  deriving (Eq)

data AttributeSelector
  = Has
  | Equals
  | WhiteSpaceContains
  | EqualsOrDashPrefix
  | Starts
  | Ends
  | Contains
  deriving (Eq, Show)

-- | Element, class, id or pseudo
data SimpleSelector = SimpleSelector
  { simpleSelectorTag :: Maybe Tag
  , simpleSelectorMaybeId :: Maybe Id
  , simpleSelectorClasses :: [Class]
  , simpleSelectorPseudos :: [Pseudo]
  , simpleSelectorAttributeSelectors :: [AttributeSelector]
  }
makeFields ''SimpleSelector

data Declaration = Declaration TS.Text Value

data Selector where
  Simple :: SimpleSelector -> Selector
  Combined :: SOp -> Selector -> SimpleSelector -> Selector

data SOp = Descendant | Child | Sibling | GeneralSibling

data KeyframeBlock
  = KeyframeBlock KeyframeSelector [Declaration]

data KeyframeSelector = From | To | KPercent Double

type Rules = [Rule]
type Declarations = [Declaration]

data Rule where
  Qualified :: Prelude -> [Declaration] -> Rule
  Keyframes :: TS.Text -> [KeyframeBlock] -> Rule
  AtRule :: TS.Text -> TS.Text -> Rules -> Rule
  FontFace :: [Declaration] -> Rule

data Prelude = Selectors [Selector]

-- * Helpers

mkRule :: Selector -> [Declaration] -> Rule
mkRule s ds = Qualified (Selectors [s]) ds

-- * Instances

instance IsString Value where
  fromString = Word . TS.pack

instance IsString SimpleSelector where
  fromString s = case s of
    '#' : rest -> fromId rest
    '.' : rest -> fromClass rest
    ':' : rest -> fromPseudoClass rest
    _ -> fromTag s
    where
      fromId s = SimpleSelector Nothing (Just $ Id $ TS.pack s) [] [] []
      fromClass s = SimpleSelector Nothing Nothing [Class $ TS.pack s] [] []
      fromPseudoClass s = SimpleSelector Nothing Nothing [] [PseudoClass (TS.pack s) Nothing] []
      fromTag s = SimpleSelector (Just $ Tag $ TS.pack s) Nothing [] [] []

instance Num Value where
  fromInteger = Int . fromInteger

deriving instance Show Tag
deriving instance Show Id
deriving instance Show Class
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
