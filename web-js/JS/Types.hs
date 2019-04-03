module JS.Types where

import Data.Kind (type (*))
import Data.Default

import qualified Prelude2 as P
import Prelude2 hiding (Bool(..), String, Eq)
import Data.Proxy
import Text.Exts
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Render (Render(renderM))
import qualified Render

-- * Operators

data UOp
  = UMinus | UPlus
  | TypeOf
  | Not
  -- | Increment post/pre | Decrement post/pre

data BOp
   = Minus | Plus | Mult | Div | Modulus
   | Eq  | NEq | EEq | NEEq
   | And | Or
   | Gt  | Lt  | GEt | LEt

type Date = ()

-- * Printers
-- instance Render Number where renderM (Number a) = tshow a
-- instance Render Bool   where renderM x = T.toLower $ tshow x

plus  = Proxy :: Proxy Plus
minus = Proxy :: Proxy Minus
mult  = Proxy :: Proxy Mult
div   = Proxy :: Proxy Div
eq    = Proxy :: Proxy Eq
neq   = Proxy :: Proxy NEq
eeq   = Proxy :: Proxy EEq
neeq  = Proxy :: Proxy NEEq
and   = Proxy :: Proxy And
or    = Proxy :: Proxy Or
gt    = Proxy :: Proxy Gt
lt    = Proxy :: Proxy Lt
gte   = Proxy :: Proxy GEt
lte   = Proxy :: Proxy LEt

data Conf
  = Indent Int
  | Minify
  deriving (P.Eq, Show, Read)

instance Default Conf where
  def = Indent 2


instance Render (Proxy Plus) where
  type Conf (Proxy Plus) = Conf
  renderM _ = pure "+"
instance Render (Proxy Minus) where
  type Conf (Proxy Minus) = Conf
  renderM _ = pure "-"
instance Render (Proxy Mult) where
  type Conf (Proxy Mult) = Conf
  renderM _ = pure "*"
instance Render (Proxy Div) where
  type Conf (Proxy Div) = Conf
  renderM _ = pure "/"
instance Render (Proxy Eq) where
  type Conf (Proxy Eq) = Conf
  renderM _ = pure "=="
instance Render (Proxy NEq) where
  type Conf (Proxy NEq) = Conf
  renderM _ = pure "!="
instance Render (Proxy EEq) where
  type Conf (Proxy EEq) = Conf
  renderM _ = pure "==="
instance Render (Proxy NEEq) where
  type Conf (Proxy NEEq) = Conf
  renderM _ = pure "!=="
instance Render (Proxy And) where
  type Conf (Proxy And) = Conf
  renderM _ = pure "&&"
instance Render (Proxy Or) where
  type Conf (Proxy Or) = Conf
  renderM _ = pure "||"
instance Render (Proxy Gt) where
  type Conf (Proxy Gt) = Conf
  renderM _ = pure ">"
instance Render (Proxy Lt) where
  type Conf (Proxy Lt) = Conf
  renderM _ = pure "<"
instance Render (Proxy GEt) where
  type Conf (Proxy GEt) = Conf
  renderM _ = pure "=>"
instance Render (Proxy LEt) where
  type Conf (Proxy LEt) = Conf
  renderM _ = pure "=<"

instance Render (Proxy UPlus) where
  type Conf (Proxy UPlus) = Conf
  renderM _ = pure "+"
instance Render (Proxy UMinus) where
  type Conf (Proxy UMinus) = Conf
  renderM _ = pure "-"
instance Render (Proxy Not) where
  type Conf (Proxy Not) = Conf
  renderM _ = pure "!"
instance Render (Proxy TypeOf) where
  type Conf (Proxy TypeOf) = Conf
  renderM _ = pure "typeof"
