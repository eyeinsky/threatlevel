module DOM.Internal where

import Prelude2

import qualified Data.Text.Lazy as TL

import qualified JS
import Render

data TagName = TagName { unTagName :: Value }
data Id      = Id { unId :: Value }
data Class   = Class { unClass :: Value }


-- * Value

data Value
  = Static TL.Text
  | Dynamic (JS.Expr ())
makeClassyPrisms ''Value

value2either v = case v of
  Static a -> Left a
  Dynamic a -> Right a

static v = case v of
  Static s -> s
  Dynamic a -> error $ show $ render a

instance IsString Value where
  fromString = Static . fromString

instance Render Value where
  renderM (Static v) = pure v

deriving instance Show TagName
instance Show Value where
  show (Static a) = show a
  show (Dynamic a) = "dynamic"
deriving instance Show Id
deriving instance Show Class
