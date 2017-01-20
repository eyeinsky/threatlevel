module Web.CSS.Internal where

import Prelude2
import Text.Format
import Text.Exts

import Numeric (showHex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Word

import Control.Monad.Writer
import Control.Monad.Identity

import Web.HTML.Core

-- * Print

class Print a where pr :: a -> TL.Text
prs x = tlshow x

-- * Syntax

-- ** Declaration

data Property = Property TL.Text
instance Print Property where pr (Property a) = a

data Value
   = Word TL.Text
   | String TL.Text

   | Percent Double
   | Em Double
   | Px Int
   | Int Int

   | ViewportHeight  Double
   | ViewportWidth   Double
   | ViewportMin     Double
   | ViewportMax     Double

   | Time Double

   | ColorHex Word32
   | ColorRGB Word8 Word8 Word8
   | ColorRGBA Word8 Word8 Word8 Double

prc i = Percent i
px i = Px i
em i = Em i
vh i   = ViewportHeight i
vw i   = ViewportWidth  i
vmin i = ViewportMin  i
vmax i = ViewportMax  i

hex a     = ColorHex a
rgb a b c = ColorRGB a b c
rgba a b c d = ColorRGBA a b c d

str = Word

instance Print Value where
   pr a = case a of
      Word a -> a
      String a -> pr (Comment "long strings unimplemented")

      Percent a -> prs a <> "%"
      Em a -> prs a <> "em"
      Px a -> prs a <> "px"
      Int a -> prs a
      Time a -> prs a <> "s"

      ViewportWidth  a -> prs a <> "vw"
      ViewportHeight a -> prs a <> "vh"
      ViewportMin    a -> prs a <> "vmin"
      ViewportMax    a -> prs a <> "vmax"

      ColorHex w32 -> "#" <> hex w32
      ColorRGB a b c -> format "rgb({},{},{})" (a,b,c)
      ColorRGBA a b c d -> format "rgba({},{},{}, {})" (a,b,c,d)
      where hex a = TL.pack $ showHex a ""

-- ** Comment

data Comment = Comment TL.Text
instance Print Comment where
   pr (Comment a) = sur "/*" "*/" a

-- ** Selector

data Pseudo = Pseudo TL.Text deriving (Eq)
instance Print Pseudo where pr (Pseudo a) = ":" <> a
instance Print TagName where pr (TagName a) = a
instance Print Id     where pr (Id     a) = "#" <> a
instance Print Class  where pr (Class  a) = "." <> a

declareFields [d|
  data SimpleSelector = SimpleSelector
    { simpleSelectorTag :: Maybe TagName -- -> SimpleSelector
    , simpleSelectorMaybeId :: Maybe Id -- -> SimpleSelector
    , simpleSelectorClasses :: [Class] -- -> SimpleSelector
    , simpleSelectorPseudos :: [Pseudo] -- -> SimpleSelector
    }
  |]

instance Print SimpleSelector where
   pr (SimpleSelector mt mi cs ps) = g mt <> g mi <> TL.concat (f cs <> f ps)
      where f = map pr
            g = maybe "" pr


data Declaration = Declaration Property Value
instance Print Declaration where
   pr (Declaration p v) = pr p <> ":" <> pr v
instance Print [Declaration] where
   pr ds = TL.concat $ map ((<>";") . pr) ds

-- ** Selector

data Selector where
  Simple :: SimpleSelector -> Selector
  Combined :: SOp -> Selector -> SimpleSelector -> Selector

data SOp = Descendant | Child | Sibling | GeneralSibling
instance Print SOp where
  pr s = case s of
    Descendant -> " "
    Child -> ">"
    Sibling -> "+"
    GeneralSibling -> "~"

instance Print Selector where
  pr s = case s of
    Simple ss -> pr ss
    Combined op s s' -> pr s <> pr op <> pr s'

-- ** Rule

type CSS = [Rule]
instance Print CSS where
   pr li = TL.unlines $ map pr li
data Rule
   = Qualified Prelude [Declaration]
   | At
instance Print Rule where
   pr r = case r of
      Qualified p ds -> pr p <> curly (pr ds)
      At -> pr (Comment "At rules not implemented..")

data Prelude = Selectors [Selector]
instance Print Prelude where
   pr (Selectors ss) = TL.intercalate "," $ map pr ss

-- ** Helpers

mkRule :: Selector -> [Declaration] -> Rule
mkRule s ds = Qualified (Selectors [s]) ds

mkDeclaration :: TL.Text -> Value -> Declaration
mkDeclaration p v = Declaration (Property p) v

-- ** Instances

deriving instance Show Rule
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

class SelectorFrom a where selFrom :: a -> Selector
instance SelectorFrom Selector where
   selFrom a = a
instance SelectorFrom SimpleSelector where
   selFrom a = Simple a
instance SelectorFrom TagName where
   selFrom a = selFrom $ SimpleSelector (Just a) Nothing [] []
instance SelectorFrom Class where
   selFrom a = selFrom $ SimpleSelector Nothing Nothing [a] []
instance SelectorFrom Id where
   selFrom a = selFrom $ SimpleSelector Nothing (Just a) [] []
instance SelectorFrom Pseudo where
   selFrom a = selFrom $ SimpleSelector Nothing Nothing [] [a]

instance IsString Class where
  fromString = Class . TL.pack

instance IsString Value where
  fromString = str . TL.pack

instance Num Value where
  fromInteger = Int . fromInteger
-- * Declaration monad

type DM = WriterT [Declaration] Identity
runDM :: DM a -> (a, [Declaration])
runDM = runIdentity . runWriterT

execDM :: DM a -> [Declaration]
execDM = snd . runDM
