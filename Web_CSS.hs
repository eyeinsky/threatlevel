module Web_CSS where

import Prelude2
import Text.Format
import Text.Exts

import Numeric (showHex)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as T
import Data.Word

-- Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity



type T = T.Text

-- * Syntax

data Rule
   = Qualified Prelude [Declaration]
   | At
instance Print Rule where
   pr r = case r of
      Qualified p ds -> pr p <> curly (T.concat $ map ((<>";") . pr) ds)
      At -> pr (Comment "At rules not implemented..")


data Prelude = Selectors [Selector]
instance Print Prelude where
   pr (Selectors ss) = T.intercalate "," $ map pr ss


data Declaration = Declaration Property Value
instance Print Declaration where
   pr (Declaration p v) = pr p <> ":" <> pr v


-- ** Selector

data Selector = Selector (Maybe Tag) [Class] [Id] [Pseudo]
instance Print Selector where
   pr (Selector mt cs is ps) = maybe "" pr mt <> T.concat (f cs <> f is <> f ps)
      where f = map pr


data Tag    = Tag T
data Id     = Id T
data Class  = Class T
data Pseudo = Pseudo T
instance Print Tag    where pr (Tag    a) =        a
instance Print Id     where pr (Id     a) = "#" <> a
instance Print Class  where pr (Class  a) = "." <> a
instance Print Pseudo where pr (Pseudo a) = ":" <> a


-- ** Declaration

data Property = Property T
instance Print Property where pr (Property a) = a


data Value
   = Word T
   | String T

   | Percent Double
   | Em Double
   | Px Int
   | Time Double

   | ColorHex Word32
   | ColorRGB Word8 Word8 Word8

hex a     = ColorHex a
rgb a b c = ColorRGB a b c

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
      where hex a = T.pack $ showHex a ""


data Comment = Comment T
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
deriving instance Show Tag
deriving instance Show Id
deriving instance Show Class
deriving instance Show Pseudo


-- * Print

class Print a where pr :: a -> T
prs x = tlshow x

-- * Convenience

tag name = Selector (Just name) [] [] []
class_ name = Selector Nothing [Class name] [] []

-- * Monad

ruleMToText :: RM () -> T.Text
ruleMToText = T.unlines . map pr . snd . runRM

type RM = WriterT [Rule] Identity
runRM = runIdentity . runWriterT :: RM a -> (a, [Rule])

type DM = WriterT [Declaration] Identity
runDM = runIdentity . runWriterT :: DM a -> (a, [Declaration])
execDM = snd . runDM :: DM a -> [Declaration]

(-#) :: Selector -> T -> Selector
Selector mt cs is ps -# str = Selector mt cs (Id str : is) ps

(-.) :: Selector -> T -> Selector
Selector mt cs is ps -. str = Selector mt (Class str : cs) is ps

(-:) :: Selector -> T -> Selector
Selector mt cs is ps -: str = Selector mt cs is (Pseudo str : ps)

e = Selector Nothing [] [] []

rule :: Selector -> DM () -> RM ()
rule s ds = tell $ [ Qualified (Selectors [s]) (runIdentity . execWriterT $ ds) ]

prop :: T -> Value -> DM ()
prop p v = tell [ Declaration (Property p) v ]


test :: RM ()
test = do
   rule (e -# "id" -. "c1" -. "c2" -: "p1" -: "p2" ) $ do
      prop "jee" $ hex 5
      prop "background-color" $ hex 7
      
{-
test = do
   
   id "smthng" $ do
      "background-color" "red"
      "position" "absolute"
      "margin" "1px 1px 1px 1px"

-}
