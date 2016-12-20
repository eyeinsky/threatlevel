module Web.CSS.Internal where

import Prelude2
import Text.Format
import Text.Exts

import Numeric (showHex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Word

-- Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

import Web.HTML.Core


-- type T = T.Text

-- * Syntax

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


data Declaration = Declaration Property Value
instance Print Declaration where
   pr (Declaration p v) = pr p <> ":" <> pr v
instance Print [Declaration] where
   pr ds = TL.concat $ map ((<>";") . pr) ds


-- ** Selector

newtype Selector = Selector { unSelector :: [SimpleSelector] }
instance Print Selector where
  pr = TL.unwords . map pr . unSelector

data SimpleSelector = SimpleSelector (Maybe TagName) (Maybe Id) [Class] [Pseudo]
instance Print SimpleSelector where
   pr (SimpleSelector mt mi cs ps) = g mt <> g mi <> TL.concat (f cs <> f ps)
      where f = map pr
            g = maybe "" pr

data Pseudo = Pseudo TL.Text
instance Print Pseudo where pr (Pseudo a) = ":" <> a

-- tag, id and class in HTML


instance Print TagName where pr (TagName a) = a
instance Print Id     where pr (Id     a) = "#" <> a
instance Print Class  where pr (Class  a) = "." <> a


-- ** Declaration

data Property = Property TL.Text
instance Print Property where pr (Property a) = a


data Value
   = Word TL.Text
   | String TL.Text

   | Percent Double
   | Em Double
   | Px Int

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
      Time a -> prs a <> "s"

      ViewportWidth  a -> prs a <> "vw"
      ViewportHeight a -> prs a <> "vh"
      ViewportMin    a -> prs a <> "vmin"
      ViewportMax    a -> prs a <> "vmax"


      ColorHex w32 -> "#" <> hex w32
      ColorRGB a b c -> format "rgb({},{},{})" (a,b,c)
      ColorRGBA a b c d -> format "rgba({},{},{}, {})" (a,b,c,d)
      where hex a = TL.pack $ showHex a ""


data Comment = Comment TL.Text
instance Print Comment where
   pr (Comment a) = sur "/*" "*/" a


-- * Instances
deriving instance Show Rule
deriving instance Show Prelude
deriving instance Show Selector
deriving instance Show SimpleSelector
deriving instance Show Declaration
deriving instance Show Property
deriving instance Show Value
deriving instance Show Comment

deriving instance Show Pseudo


-- * Print

class Print a where pr :: a -> TL.Text
prs x = tlshow x


-- * Convenience

class SelectorFrom a where selFrom :: a -> Selector
instance SelectorFrom Selector where
   selFrom a = a
instance SelectorFrom SimpleSelector where
   selFrom a = Selector [a]
instance SelectorFrom TagName where
   selFrom a = selFrom $ SimpleSelector (Just a) Nothing [] []
instance SelectorFrom Class where
   selFrom a = selFrom $ SimpleSelector Nothing Nothing [a] []
instance SelectorFrom Id where
   selFrom a = selFrom $ SimpleSelector Nothing (Just a) [] []
instance SelectorFrom Pseudo where
   selFrom a = selFrom $ SimpleSelector Nothing Nothing [] [a]

-- * Monad

ruleMToText :: RM () -> TL.Text
ruleMToText = TL.unlines . map pr . snd . runRM

type RM = WriterT [Rule] Identity
runRM = runIdentity . runWriterT :: RM a -> (a, [Rule])

type DM = WriterT [Declaration] Identity
runDM = runIdentity . runWriterT :: DM a -> (a, [Declaration])
execDM = snd . runDM :: DM a -> [Declaration]


(-#) :: SimpleSelector -> TL.Text -> SimpleSelector
SimpleSelector mt is cs ps -# str = SimpleSelector mt (Just (Id str)) cs ps

(-.) :: SimpleSelector -> TL.Text -> SimpleSelector
SimpleSelector mt is cs ps -. str = SimpleSelector mt is (Class str : cs) ps

(-:) :: SimpleSelector -> TL.Text -> SimpleSelector
SimpleSelector mt is cs ps -: str = SimpleSelector mt is cs (Pseudo str : ps)


rule :: SelectorFrom a => a -> DM () -> RM ()
rule s ds = tell $ [ Qualified (Selectors [selFrom s]) (runIdentity . execWriterT $ ds) ]

prop :: TL.Text -> Value -> DM ()
prop p v = tell [ Declaration (Property p) v ]


{-
      -}
test :: RM ()
test = let
    e = SimpleSelector Nothing Nothing [] []
  in do
  rule (e -# "id" -. "c1" -. "c2" -: "p1" -: "p2") $ do
      prop "jee" $ hex 5
      prop "background-color" $ hex 7

toRules :: RM a -> [Rule]
toRules = snd . runRM


-- Common

resetCSS = do
   rule (TagName "body") $ nopad >> nomarg
   rule (TagName "div") $ nopad >> nomarg
   where
      nopad = prop "padding" $ px 0
      nomarg = prop "margin" $ px 0

instance IsString Class where
  fromString = Class . TL.pack

instance IsString Value where
  fromString = str . TL.pack
