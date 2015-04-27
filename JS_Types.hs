module JS_Types where

import qualified Prelude2 as P
import Prelude2 hiding (Bool(..), String, Eq)
import Text.Exts
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Base


newtype Bool = Bool P.Bool
newtype Number = Number P.Double
newtype NumberI = NumberI P.Integer
newtype String = String T.Text
newtype Array a = Array [a]
data Object = Object
data Regex = Regex T.Text T.Text

-- * Operators

data UOp = UMinus | UPlus | TypeOf | Not
data BOp
   = Minus | Plus | Mult | Div
   | Eq  | NEq | EEq | NEEq
   | And | Or
   | Gt  | Lt  | GEt | LEt


-- * Instances

class O (ty :: *) (op :: BOp) where type D ty op :: *
class U (ty :: *) (op :: UOp) where

instance O Bool And where type D Bool And = Bool
instance O Bool Or  where type D Bool Or = Bool
instance U Bool Not

instance O Number Plus  where type D Number Plus = Number
instance O Number Minus where type D Number Minus = Number
instance O Number Mult  where type D Number Mult = Number
instance O Number Div   where type D Number Div = Number
instance O Number Eq    where type D Number Eq = Bool
instance O Number NEq   where type D Number NEq = Bool
instance O Number EEq   where type D Number EEq = Bool
instance O Number NEEq  where type D Number NEEq = Bool

instance U Number UPlus  where -- type D Number UPlus = Number
instance U Number UMinus where -- type D Number UMinus = Number

instance O String Plus  where type D String Plus = String 

instance U a TypeOf 



-- * Printers
-- instance E Number where ev (Number a) = tshow a
-- instance E Bool   where ev x = T.toLower $ tshow x

data Proxy a = Proxy
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
instance E (Proxy Plus) where ev _ = "+"
instance E (Proxy Minus) where ev _ = "-"
instance E (Proxy Mult) where ev _ = "*"
instance E (Proxy Div) where ev _ = "/"
instance E (Proxy Eq) where ev _ = "=="
instance E (Proxy NEq) where ev _ = "!="
instance E (Proxy EEq) where ev _ = "==="
instance E (Proxy NEEq) where ev _ = "!=="
instance E (Proxy And) where ev _ = "&&"
instance E (Proxy Or) where ev _ = "||"
instance E (Proxy Gt) where ev _ = ">"
instance E (Proxy Lt) where ev _ = "<"
instance E (Proxy GEt) where ev _ = "=>"
instance E (Proxy LEt) where ev _ = "=<"

instance E (Proxy UPlus) where ev _ = "+"
instance E (Proxy UMinus) where ev _ = "-"
instance E (Proxy Not) where ev _ = "!"
instance E (Proxy TypeOf) where ev _ = "typeof"


