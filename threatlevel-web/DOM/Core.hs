{-# LANGUAGE UndecidableInstances #-}
module DOM.Core where

import X.Prelude

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import JS
import Render
import CSS hiding (Value)

newtype TagName = TagName { unTagName :: Value }
newtype Id      = Id { unId :: Value }
newtype Class   = Class { unClass :: Value }

-- | Stubs
data Tag
data Window
data DocumentFragment
data Location
data Document

-- * Objects

window :: Expr Window
window = ex "window"

document :: Expr Document
document = ex "document"

location :: Expr Location
location = window !. "location"

instance Semigroup (Expr DocumentFragment) where
  (<>) = error "Not implemented: Semigroup (Expr DocumentFragment)"
instance Monoid (Expr DocumentFragment) where
  mempty = document JS.!/ "createDocumentFragment"

-- * Value

data Value
  = Static TS.Text
  | Dynamic (JS.Expr ())
makeClassyPrisms ''Value

value2either :: Value -> Either TS.Text (Expr ())
value2either v = case v of
  Static a -> Left a
  Dynamic a -> Right a

static :: Value -> TS.Text
static v = case v of
  Static s -> s
  Dynamic a -> error $ show $ render Minify a

-- * Instances

instance IsString Value where
  fromString = Static . fromString

instance Render Value where
  renderM v' = case v' of
    Static v -> pure $ TL.fromStrict v
    _ -> error "XML.Core: can't render dynamic attribute"

instance JS.ToExpr Value where
  lit v = case v of
    Static a -> JS.lit a
    Dynamic a -> Cast a

deriving instance Show TagName
instance IsString TagName where
  fromString = TagName . fromString

instance Show Value where
  show (Static a) = show a
  show (Dynamic _) = "dynamic"
deriving instance Show Id
deriving instance Show Class

-- ** SimpleSelectorFrom

instance SimpleSelectorFrom TagName where
  ssFrom a = SimpleSelector (Just $ static $ coerce a) Nothing [] [] []
instance SimpleSelectorFrom [Class] where
  ssFrom a = SimpleSelector Nothing Nothing (map (static . coerce) a) [] []
instance SimpleSelectorFrom Class where
  ssFrom a = ssFrom [a]
instance SimpleSelectorFrom Id where
  ssFrom a = SimpleSelector Nothing (Just $ static $ coerce a) [] [] []

-- ** RenderJSM

class RenderJSM a where
  renderJSM :: a -> JS.M r (JS.Expr Tag)

class (Render a, RenderJSM a) => Both a
instance (Render a, RenderJSM a) => Both a
