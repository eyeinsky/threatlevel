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
      Qualified p ds -> pr p <> curly (T.intercalate ";" $ map pr ds)
      At -> pr (Comment "At rules not implemented..")


data Prelude = Selectors [Selector]
instance Print Prelude where
   pr (Selectors ss) = T.intercalate "," $ map pr ss


data Declaration = Declaration Property Value
instance Print Declaration where
   pr (Declaration p v) = pr p <> ":" <> pr v


-- ** Selector

data Selector = Selector (Maybe STag) [SClass] [SId] [SPseudo]
instance Print Selector where
   pr (Selector mt cs is ps) = maybe "" pr mt <> T.concat (f cs <> f is <> f ps)
      where f = map pr


data STag    = Tag T
data SId     = Id T
data SClass  = Class T
data SPseudo = Pseudo T
instance Print STag    where pr (Tag    a) =        a
instance Print SId     where pr (Id     a) = "#" <> a
instance Print SClass  where pr (Class  a) = "." <> a
instance Print SPseudo where pr (Pseudo a) = ":" <> a


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
      where hex a = tlshow $ showHex a ""


data Comment = Comment T
instance Print Comment where
   pr (Comment a) = sur "/*" "*/" a


-- * Print

class Print a where pr :: a -> T
prs x = tlshow x


-- * Monad

{-
test = do
   
   id "smthng" $ do
      "background-color" "red"
      "position" "absolute"
      "margin" "1px 1px 1px 1px"

-}
