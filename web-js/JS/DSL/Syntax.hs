{-# OPTIONS_GHC -Wno-orphans #-}
module JS.DSL.Syntax where

import Common.Prelude
import Data.Time
import Prelude (Floating(..), Fractional(..))
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as TL
import qualified Data.Aeson as A

import JS.Syntax

-- * Expressional keywords

this :: Expr a
this = ex "this"

true :: Expr Bool
true = ex "true"

false :: Expr Bool
false = ex "false"

arguments :: Expr a
arguments = ex "arguments"

typeOf :: Expr a -> Expr String
typeOf = Op . OpUnary TypeOf

instanceof :: Expr a -> Expr b -> Expr Bool
e1 `instanceof` e2 = Op $ OpBinary Instanceof e1 e2

-- * Operators (untyped)

e1 .==  e2 = Op $ OpBinary   Eq e1 e2
e1 .=== e2 = Op $ OpBinary  EEq e1 e2
e1 .!=  e2 = Op $ OpBinary  NEq e1 e2
e1 .!== e2 = Op $ OpBinary NEEq e1 e2
infix 4 .==
infix 4 .===
infix 4 .!=
infix 4 .!==

e1 .&& e2 = Op $ OpBinary And e1 e2
e1 .|| e2 = Op $ OpBinary Or e1 e2
infixr 3 .&&
infixr 2 .||

e1 .<  e2  = Op $ OpBinary Lt e1 e2
e1 .>  e2  = Op $ OpBinary Gt e1 e2
e1 .<= e2 = Op $ OpBinary LEt e1 e2
e1 .>= e2 = Op $ OpBinary GEt e1 e2
infix 4 .<
infix 4 .>
infix 4 .<=
infix 4 .>=

e1 % e2 = Op $ OpBinary Modulus e1 e2
infixl 7  %

not :: Expr Bool -> Expr Bool
not = Op . OpUnary Not

-- * Instances

-- ** Literals

class ToExpr a where lit :: a -> Expr b
instance ToExpr (Expr a) where lit = Cast
instance ToExpr Int      where lit = Lit . Integer . toInteger
instance ToExpr Integer  where lit = Lit . Integer
instance ToExpr Rational where lit = Lit . Double . fromRational
instance ToExpr Double   where lit = Lit . Double
instance ToExpr Bool     where lit = Lit . Bool
instance ToExpr TS.Text  where lit = Lit . String
instance ToExpr TL.Text  where lit = lit . TL.toStrict
instance ToExpr String   where lit = lit . TL.pack
instance ToExpr A.Value where
  lit v = A.encode v ^. TL.utf8 & Raw

instance {-# OVERLAPPABLE #-} ToExpr a => ToExpr [a] where
   lit = Lit . Array . map lit

-- ** Object

instance ToExpr v => ToExpr [(TS.Text, v)] where
   lit li = Lit $ Object $ map f li
      where f (k, v) = (Left $ Name k, lit v)
ck f = lit . map (first f)
instance ToExpr v => ToExpr [(TL.Text, v)] where
   lit = ck TL.toStrict
instance ToExpr v => ToExpr [(String, v)] where
  lit = ck TS.pack
instance ToExpr v => ToExpr [(Expr k, v)] where
   lit li = Lit $ Object $ map f li
      where f (k, v) = (Right $ Cast k, lit v)


instance IsString (Expr a) where
   fromString s = lit s

instance ToExpr a => ToExpr (Maybe a) where
  lit = maybe Null lit

instance ToExpr UTCTime where
  lit t = lit $ formatTime defaultTimeLocale format t
    where
      format = iso8601DateFormat (Just "%H:%M:%S%QZ")
      -- Prints ISO8601, e.g "2019-11-04T15:42:18.608734Z"

instance ToExpr Day where
  lit t = lit $ show t

-- ** Regex

data RegExp
regex :: TS.Text -> TS.Text -> Expr RegExp
regex str opts = Lit $ RegExp str opts

toRegex :: Expr a -> TS.Text -> Expr RegExp
toRegex str mod = call (ex "RegExp") [str, lit mod]

-- ** Array

(!-) :: ToExpr b => Expr a -> b -> Expr c
(!-) a b = Arr a (lit b)

instance {-# OVERLAPPABLE #-} Semigroup (Expr [a]) where
  a <> b = a !// "concat" $ b
instance {-# OVERLAPPABLE #-} Monoid (Expr [a]) where
  mempty = Lit $ Array []

data Object

instance {-# OVERLAPPABLE #-} Semigroup (Expr Object) where
  a <> b = a !// "concat" $ b
instance {-# OVERLAPPABLE #-} Monoid (Expr Object) where
  mempty = Lit $ Object []

instance {-# OVERLAPPABLE #-} Semigroup (Expr a) where
  a <> b = call (ex "Object" !. "assign") [mempty :: Expr Object, Cast a, Cast b]
instance {-# OVERLAPPABLE #-} Monoid (Expr a) where
  mempty = Lit $ Object []

instance {-# OVERLAPPABLE #-} Semigroup (Expr String) where
  a <> b = a + b
instance Monoid (Expr String) where
  mempty = ""

-- ** Math

math name = ex "Math" !. name

instance Num (Expr a) where
   fromInteger s = lit s
   e1 + e2 = Op $ OpBinary Plus e1 e2
   e1 - e2 = Op $ OpBinary Minus e1 e2
   e1 * e2 = Op $ OpBinary Mult e1 e2
   negate n = 0 - n
   abs = call1 (math "abs")
   signum = call1 (math "sign")

instance Fractional (Expr a) where
   fromRational s = lit s
   e1 / e2 = Op $ OpBinary Div e1 e2

instance Floating (Expr a) where
  pi = lit pi
  exp = todo
  log = todo
  sin = todo
  cos = todo
  asin = todo
  acos = todo
  atan n = call1 (math "atan") n
  sinh = todo
  cosh = todo
  asinh = todo
  acosh = todo
  atanh = todo

-- * Function

a !/ b = call0 (a !. b)
a !// b = call1 (a !. b)
