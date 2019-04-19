{-# LANGUAGE UndecidableInstances #-}
module DOM.Core where

import Prelude2

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import qualified JS
import Render
import JS.Render

data TagName = TagName { unTagName :: Value }
data Id      = Id { unId :: Value }
data Class   = Class { unClass :: Value }

-- | Stubs
data Tag
data Window
data DocumentFragment
data Location

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
  renderM (Static v) = pure $ TL.fromStrict v

deriving instance Show TagName
instance Show Value where
  show (Static a) = show a
  show (Dynamic a) = "dynamic"
deriving instance Show Id
deriving instance Show Class

-- * RenderJSM

class RenderJSM a where
  renderJSM :: a -> JS.M r (JS.Expr Tag)

class (Render a, RenderJSM a) => Both a
instance (Render a, RenderJSM a) => Both a
