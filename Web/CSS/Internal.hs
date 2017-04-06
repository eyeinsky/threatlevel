module Web.CSS.Internal where

import Prelude2 hiding (unlines, intercalate, concat)
import Text.Format

import Numeric (showHex)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Data.Word

import Control.Monad.Writer
import Control.Monad.Identity

import Web.HTML.Core hiding (Value)
import Render

-- * Syntax

-- ** Declaration

data Property = Property TL.Text
instance Render Property where renderM (Property a) = pure a

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

instance Render Value where
   renderM a = case a of
      Word a -> pure a
      String a -> renderM (Comment "long strings unimplemented")

      Percent a -> pure $ tshow a <> "%"
      Em a -> pure $ p a <> "em"
      Px a -> pure $ p a <> "px"
      Int a -> pure $ p a
      Time a -> pure $ p a <> "s"

      ViewportWidth  a -> pure $ p a <> "vw"
      ViewportHeight a -> pure $ p a <> "vh"
      ViewportMin    a -> pure $ p a <> "vmin"
      ViewportMax    a -> pure $ p a <> "vmax"

      ColorHex w32 -> pure $ "#" <> hex w32
      ColorRGB a b c -> pure $ format "rgb({},{},{})" (a,b,c)
      ColorRGBA a b c d -> pure $ format "rgba({},{},{}, {})" (a,b,c,d)
      where
        hex a = TL.pack $ showHex a ""
        p = tshow

-- ** Comment

data Comment = Comment TL.Text
instance Render Comment where
   renderM (Comment a) = pure $ sur "/*" "*/" a

-- ** Selector

data Pseudo = Pseudo TL.Text deriving (Eq)
instance Render Pseudo where renderM (Pseudo a) = pure $ ":" <> a
instance Render TagName where renderM (TagName a) = renderM a
instance Render Id     where renderM (Id     a) = ("#" <>) <$> renderM a
instance Render Class  where renderM (Class  a) = ("." <>) <$> renderM a

declareFields [d|
  data SimpleSelector = SimpleSelector
    { simpleSelectorTag :: Maybe TagName
    , simpleSelectorMaybeId :: Maybe Id
    , simpleSelectorClasses :: [Class]
    , simpleSelectorPseudos :: [Pseudo]
    }
  |]

instance Render SimpleSelector where
   renderM (SimpleSelector mt mi cs ps)
     = g mt <+> g mi <+> (concat <$> (f cs <+> f ps))
      where f = mapM renderM
            g = maybe (pure "") renderM


data Declaration = Declaration Property Value
instance Render Declaration where
   renderM (Declaration p v) = mseq [renderM p, pure ":", renderM v]
instance Render [Declaration] where
   renderM ds = concat . map (<> ";") <$> mapM renderM ds

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

-- ** Rule

type CSS = [Rule]
instance Render CSS where
   renderM li = unlines <$> mapM renderM (filter (not . isEmpty) li)
     where
       isEmpty r = case r of
         Qualified _ [] -> True
         _ -> False

data Rule
   = Qualified Prelude [Declaration]
   | At
instance Render Rule where
   renderM r = case r of
      Qualified p ds -> renderM p <+> (curly <$> (renderM ds))
      At -> renderM (Comment "At rules not implemented..")

data Prelude = Selectors [Selector]
instance Render Prelude where
   renderM (Selectors ss) = intercalate "," <$> mapM renderM ss

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
-- * Declaration monad

type DM = WriterT [Declaration] Identity
runDM :: DM a -> (a, [Declaration])
runDM = runIdentity . runWriterT

execDM :: DM a -> [Declaration]
execDM = snd . runDM
