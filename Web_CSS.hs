module Web_CSS
   ( module Web_CSS
   , module Web_HTML
   )
   where

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

import Web_HTML


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
      Qualified p ds -> pr p <> curly (TL.concat $ map ((<>";") . pr) ds)
      At -> pr (Comment "At rules not implemented..")


data Prelude = Selectors [Selector]
instance Print Prelude where
   pr (Selectors ss) = TL.intercalate "," $ map pr ss


data Declaration = Declaration Property Value
instance Print Declaration where
   pr (Declaration p v) = pr p <> ":" <> pr v


-- ** Selector

data Selector = Selector (Maybe Tag) (Maybe Id) [Class] [Pseudo]
instance Print Selector where
   pr (Selector mt mi cs ps) = g mt <> g mi <> TL.concat (f cs <> f ps)
      where f = map pr
            g = maybe "" pr

data Pseudo = Pseudo TL.Text
instance Print Pseudo where pr (Pseudo a) = ":" <> a

-- tag, id and class in HTML


instance Print Tag    where pr (Tag    a) =        a
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
   | Time Double

   | ColorHex Word32
   | ColorRGB Word8 Word8 Word8
   | ColorRGBA Word8 Word8 Word8 Double

prc i = Percent i
px i = Px i
em i = Em i

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

      ColorHex w32 -> "#" <> hex w32
      ColorRGB a b c -> format "rgb({},{},{})" (a,b,c)
      ColorRGBA a b c d -> format "rgb({},{},{}, {})" (a,b,c, a)
      where hex a = TL.pack $ showHex a ""


data Comment = Comment TL.Text
instance Print Comment where
   pr (Comment a) = sur "/*" "*/" a


-- * Instances
deriving instance Show Rule
deriving instance Show Prelude
deriving instance Show Selector
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
instance SelectorFrom Tag where 
   selFrom a = Selector (Just a) Nothing [] []
instance SelectorFrom Class where 
   selFrom a = Selector Nothing Nothing [a] []
instance SelectorFrom Id where 
   selFrom a = Selector Nothing (Just a) [] []
instance SelectorFrom Pseudo where
   selFrom a = Selector Nothing Nothing [] [a]


-- * Monad

ruleMToText :: RM () -> TL.Text
ruleMToText = TL.unlines . map pr . snd . runRM

type RM = WriterT [Rule] Identity
runRM = runIdentity . runWriterT :: RM a -> (a, [Rule])

type DM = WriterT [Declaration] Identity
runDM = runIdentity . runWriterT :: DM a -> (a, [Declaration])
execDM = snd . runDM :: DM a -> [Declaration]

(-#) :: Selector -> TL.Text -> Selector
Selector mt is cs ps -# str = Selector mt (Just (Id str)) cs ps

(-.) :: Selector -> TL.Text -> Selector
Selector mt is cs ps -. str = Selector mt is (Class str : cs) ps

(-:) :: Selector -> TL.Text -> Selector
Selector mt is cs ps -: str = Selector mt is cs (Pseudo str : ps)

e = Selector Nothing Nothing [] []

rule :: SelectorFrom a => a -> DM () -> RM ()
rule s ds = tell $ [ Qualified (Selectors [selFrom s]) (runIdentity . execWriterT $ ds) ]

prop :: TL.Text -> Value -> DM ()
prop p v = tell [ Declaration (Property p) v ]


test :: RM ()
test = do
   rule (e -# "id" -. "c1" -. "c2" -: "p1" -: "p2") $ do
      prop "jee" $ hex 5
      prop "background-color" $ hex 7
      
toRules :: RM a -> [Rule]
toRules = snd . runRM


-- Common

resetCSS = do
   rule (Tag "body") $ nopad >> nomarg
   rule (Tag "div") $ nopad >> nomarg
   where 
      nopad = prop "padding" $ px 0
      nomarg = prop "margin" $ px 0
