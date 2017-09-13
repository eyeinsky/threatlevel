module XML
  ( module XML
  , classes
  ) where

import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as HM
import Control.Monad.Writer

import Pr hiding (id)
import Prelude2.Has (HasId(..))
import qualified JS
import DOM.Core
import DOM.Event


data XML ns a where
  Element :: TagName -> a -> [XML ns a] -> XML ns a
  Text :: TL.Text -> XML ns a
  Dyn :: JS.Expr a -> XML ns a

makePrisms ''XML
contents = _Element._3

instance IsString (XML ns a) where
   fromString str = Text $ TL.pack str

instance IsString (Writer [XML ns a] ()) where
   fromString str = text $ TL.pack str

-- ** Shorthands

tag :: Monoid a => Value -> XML n a
tag str = Element (TagName str) mempty []

dyn :: JS.Expr b -> Writer [XML ns a] ()
dyn expr = tell [Dyn (JS.Cast expr)]

text :: TL.Text -> Writer [XML ns a] ()
text tl = tell [Text tl]

-- * Attribute

data Attribute where
  Custom :: TL.Text -> TL.Text -> Attribute
  OnEvent :: Event event => event -> JS.Expr a -> Attribute
  Data ::  TL.Text -> TL.Text -> Attribute
  AttrClass :: [Class] -> Attribute
  AttrId :: Id -> Attribute

declareFields [d|
  data AttributeSet = AttributeSet
    { attributeSetId :: Maybe Id
    , attributeSetClasses :: [Class]
    , attributeSetAttrs :: HM.HashMap TL.Text Attribute
    }
  |]

instance Monoid AttributeSet where
  mempty = AttributeSet Nothing mempty mempty
  mappend _ _ = todoMsg "mappend not implemented for AttributeSet"

id_ :: Id -> Attribute
id_ id = AttrId id :: Attribute

cls_ :: [Class] -> Attribute
cls_ cs = AttrClass cs

-- * Writer monad

type XMLA n = XML n AttributeSet
type XMLM n = Writer [XMLA n] ()

-- * Add attributes with !

class Attributable a where
  (!-) :: a -> Attribute -> a

instance Attributable AttributeSet where
  (!-) a attr = case attr of
    AttrClass cs -> a & classes %~ (cs <>)
    AttrId id' -> a & id .~ Just id'
    Custom k v -> a & attrs %~ (HM.insert k attr)
    OnEvent e v -> a & attrs %~ (HM.insert (toOn e) attr)
    Data e v -> a & attrs %~ (HM.insert e attr)

instance Attributable (XMLA n) where
  (!-) e attr = e & _Element._2 %~ (!- attr)

instance Attributable (XMLM n) where
  (!-) a attr = case execWriter a of
    e : rest -> tell (e !- attr : rest)
    _ -> return ()

instance Attributable (XMLM n -> XMLM n) where
  (!-) a attr = case execWriter (a $ pure ()) of
    e : rest -> \ x -> let
        ct = execWriter x
        e' = (e !- attr) & contents .~ ct
      in tell (e' : rest)
    _ -> a


-- * Exclamatable

class Exclamatable e a where
  (!) :: e -> a -> e

instance Exclamatable (XMLM n) Id where
  (!) e id = e !- id_ id
instance Exclamatable (XMLM n -> XMLM n) Id where
  (!) e id = e !- id_ id

instance Exclamatable (XMLM n) [Class] where
  (!) e cs = e !- cls_ cs
instance Exclamatable (XMLM n -> XMLM n) [Class] where
  (!) e cs = e !- cls_ cs

instance Exclamatable (XMLM n) Class where
  (!) e c = e !- cls_ [c]
instance Exclamatable (XMLM n -> XMLM n) Class where
  (!) e c = e !- cls_ [c]

instance Exclamatable (XMLM n) Attribute where
  (!) e c = e !- c
instance Exclamatable (XMLM n -> XMLM n) Attribute where
  (!) e c = e !- c
