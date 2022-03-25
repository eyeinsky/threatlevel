module XML.Core where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as HM
import Control.Monad.Writer

import X.Prelude hiding (id)
import qualified JS
import DOM.Core hiding (Id(..), Class(..))
import qualified DOM.Core as DOM.Core
import JS.Event
import qualified Render

data XML ns a c where
  Element :: TagName -> a -> [XML ns a c] -> XML ns a c
  Text :: TL.Text -> XML ns a c
  Raw :: TL.Text -> XML ns a c
  Dyn :: JS.Expr a -> XML ns a c
  Embed
    :: ( c (XML ns' a' c)
       , Render.Conf (XML ns a c) ~ Render.Conf (XML ns' a' c)
       )
    => XML ns' a' c -> XML ns a c

makePrisms ''XML
contents = _Element._3

instance IsString (XML ns a c) where
   fromString str = Text $ TL.pack str

instance IsString (Writer [XML ns a c] ()) where
   fromString str = text $ TL.pack str

-- ** Shorthands

dyn :: JS.Expr b -> Writer [XML ns a c] ()
dyn expr = tell [Dyn (JS.Cast expr)]

text :: TL.Text -> Writer [XML ns a c] ()
text tl = tell [Text tl]

raw :: TL.Text -> Writer [XML ns a c] ()
raw tl = tell [Raw tl]

embed
  :: ( c (XML ns' a' c), c (XML ns a c)
     , Render.Conf (XML ns' a' c) ~ Render.Conf (XML ns a c)
     )
  => Writer [XML ns' a' c] a1 -> Writer [XML ns a c] ()
embed xml = tell $ map Embed $ execWriter xml

-- * Attribute

data Attribute where
  Custom :: TS.Text -> Value -> Attribute
  Class :: [Value] -> Attribute
  Id :: Value -> Attribute
  Data :: TS.Text -> Value -> Attribute
  Boolean :: TS.Text -> Bool -> Attribute
  On :: Event event => event -> JS.Expr a -> Attribute

declareFields [d|
  data AttributeSet = AttributeSet
    { attributeSetId :: Maybe Value
    , attributeSetClasses :: [Value]
    , attributeSetAttrs :: HM.HashMap TS.Text Attribute
    }
  |]

instance Semigroup AttributeSet where
  _ <> _ = todoMsg "mappend not implemented for AttributeSet"
instance Monoid AttributeSet where
  mempty = AttributeSet Nothing mempty mempty

-- * Writer monad

type XMLA ns c = XML ns AttributeSet c
type XMLM ns c = Writer [XMLA ns c] ()

instance Semigroup (XMLM ns c) where
  a <> b = a >> b

instance Monoid (XMLM ns c) where
  mempty = pure ()

-- * Add attributes with !

class Attributable a where
  (!-) :: a -> Attribute -> a

instance Attributable AttributeSet where
  (!-) a attr = case attr of
    Custom k _ -> a & attrs %~ (HM.insert k attr)
    Class cs -> a & classes %~ (cs <>)
    Id v -> a & id .~ Just v
    Data k _ -> a & attrs %~ (HM.insert k attr)
    Boolean k _ -> a & attrs %~ (HM.insert k attr)
    On e _ -> a & attrs %~ (HM.insert (toOn e) attr)

instance Attributable (XMLA ns c) where
  (!-) e attr = e & _Element._2 %~ (!- attr)

instance Attributable (XMLM ns c) where
  (!-) a attr = case execWriter a of
    e : rest -> tell (e !- attr : rest)
    _ -> return ()

instance Attributable (XMLM ns c -> XMLM ns c) where
  (!-) a attr = case execWriter (a $ pure ()) of
    e : rest -> \ x -> let
        ct = execWriter x
        e' = (e !- attr) & contents .~ ct
      in tell (e' : rest)
    _ -> a


-- * Exclamatable

class Exclamatable e a where
  (!) :: e -> a -> e

instance Exclamatable (XMLM ns c) DOM.Core.Id where
  (!) e id = e !- Id (coerce id)
instance Exclamatable (XMLM ns c -> XMLM ns c) DOM.Core.Id where
  (!) e id = e !- Id (coerce id)

instance Exclamatable (XMLM ns c) [DOM.Core.Class] where
  (!) e cs = e !- Class (map coerce cs)
instance Exclamatable (XMLM ns c -> XMLM ns c) [DOM.Core.Class] where
  (!) e cs = e !- Class (map coerce cs)

instance Exclamatable (XMLM ns c) DOM.Core.Class where
  (!) e c = e !- Class [coerce c]
instance Exclamatable (XMLM ns c -> XMLM ns c) DOM.Core.Class where
  (!) e c = e !- Class [coerce c]

instance Exclamatable (XMLM ns c) Attribute where
  (!) e c = e !- c
instance Exclamatable (XMLM ns c -> XMLM ns c) Attribute where
  (!) e c = e !- c

-- * Helpers

tag
  :: forall m ns c . MonadWriter [XML ns AttributeSet c] m
  => Value -> XMLM ns c -> m ()
tag name mcontent = tell [theTag]
  where theTag = Element (TagName name) mempty (execWriter mcontent)

tag' :: Monoid a => Value -> XML n a c
tag' str = Element (TagName str) mempty []
