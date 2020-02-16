{-# LANGUAGE UndecidableInstances #-}
module DOM.Core where

import X.Prelude

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import qualified JS
import Render
import JS.Syntax

data TagName = TagName { unTagName :: Value }
data Id      = Id { unId :: Value }
data Class   = Class { unClass :: Value }

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

value2either v = case v of
  Static a -> Left a
  Dynamic a -> Right a

static v = case v of
  Static s -> s
  Dynamic a -> error $ show $ render Minify a

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
instance Show Value where
  show (Static a) = show a
  show (Dynamic _) = "dynamic"
deriving instance Show Id
deriving instance Show Class

-- * RenderJSM

class RenderJSM a where
  renderJSM :: a -> JS.M r (JS.Expr Tag)

class (Render a, RenderJSM a) => Both a
instance (Render a, RenderJSM a) => Both a
