module JS.Core
   (

   -- | JS.Syntax reexports
     Statement(BareExpr)
   , Expr(Undefined, Null, Par, Lit, Cast, AnonFunc, Raw)
   , Attr(..), Name(..)
   , Literal(..)
   , Code
   , call, call0, call1, (!.), (!-), (.!), (=:), ex

   -- | Typed functions
   -- , a1, a2, a3, a4, a5 -- apply typed function to tuple-arguments
   -- , (-/)

   , module JS.DSL
   , module JS.Core
   )
   where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Prelude (Float, fromRational, toRational, Fractional, Rational)

import Prelude2 hiding ((.-), for, (.=), (.>), not)
import qualified Prelude2 as Pr
import Prelude (Fractional, fromRational)
import Data.Default

import qualified Data.Set as S
import qualified Data.Hashable as H

import Control.Monad.State

import Render (render)
import JS.Types as JT
import JS.Syntax
import JS.Render hiding ((=:))
import JS.DSL

pr :: M r a -> IO ()
pr = TL.putStrLn . render (Indent 2) . snd . fst . runM def def

-- * Literals

class    ToLiteral a       where lit :: a -> Expr b
instance ToLiteral (Expr a) where lit = Cast
instance ToLiteral Int     where lit = Lit . Integer . toInteger
instance ToLiteral Integer where lit = Lit . Integer
instance ToLiteral Rational where lit = Lit . Double . fromRational
instance ToLiteral Double  where lit = Lit . Double
instance ToLiteral Bool    where lit = Lit . Bool
instance ToLiteral TS.Text  where lit = Lit . String
instance ToLiteral TL.Text where lit = lit . TL.toStrict
instance ToLiteral String  where lit = lit . TL.pack

instance ToLiteral [ Expr a ] where
   lit = Lit . Array . map Cast

-- * Literal objects

instance ToLiteral v => ToLiteral [(TS.Text, v)] where
   lit li = Lit $ Object $ map f li
      where f (k, v) = (Left $ Name k, lit v)

ck f = lit . map (first f)
instance ToLiteral v => ToLiteral [(TL.Text, v)] where
   lit = ck TL.toStrict
instance ToLiteral v => ToLiteral [(String, v)] where
  lit = ck TS.pack

instance IsString (Expr a) where
   fromString s = lit s

(!-) :: ToLiteral b => Expr a -> b -> Expr c -- TODO add types
(!-) a b = Arr a (lit b)

-- * Modules

lib :: M r (Expr a) -> M r (Expr a)
lib mcode = let
    codeText = render Minify . snd . fst . runM def def $ mcode -- fix: take config from somewhere
    codeHash = H.hash codeText
    nameExpr = EName $ Name $ "h" <> TS.replace "-" "_" (TL.toStrict $ tshow codeHash)
  in do
  set <- gets (^.library)
  when (Pr.not $ codeHash `S.member` set) $ do
    f <- mcode
    nameExpr .= f
    modify (library %~ S.insert codeHash)
  return nameExpr

instance Render (M r a) where
  type Conf (M r a) = JS.Render.Conf
  renderM = renderM . snd . fst . runM def def

math name = ex "Math" !. name

-- ** Operators (untyped)

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

e1 .+ e2 = Op $ OpBinary Plus e1 e2
e1 .- e2 = Op $ OpBinary Minus e1 e2
e1 .* e2 = Op $ OpBinary Mult e1 e2
e1 ./ e2 = Op $ OpBinary Div e1 e2
e1 .% e2 = Op $ OpBinary Modulus e1 e2
(%) = (.%)

infixl 6 .+
infixl 6 .-
infixl 7 .*
infixl 7 ./
infixl 7 .%
infixl 7  %

instance Num (Expr a) where
   fromInteger s = lit s
   (+) = (.+)
   (-) = (.-)
   (*) = (.*)
   negate n = 0 - n
   abs = call1 (math "abs")
   signum = call1 (math "sign")
instance Fractional (Expr a) where
   fromRational s = lit s
   (/) = (./)

a .+= b = a .= (a .+ b)
a .-= b = a .= (a .- b)
a .*= b = a .= (a .* b)
a ./= b = a .= (a ./ b)
