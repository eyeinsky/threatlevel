module JS.API where

import Prelude2 hiding ((.-), (.=), (.>))
import Prelude (Floating(..))
import JS.DSL

-- * Object

this :: Expr a
this = ex "this"

-- * Bool

true :: Expr Bool
true = ex "true"

false :: Expr Bool
false = ex "false"

-- * Array

-- ** Mutating

push :: Expr a -> Expr [a] -> Expr c
push a as = call1 (as !. "push") a

pop :: Expr [a] -> Expr a
pop as = call0 (as !. "pop")

join :: Expr String -> Expr [a] -> Expr b
join str arr = call1 (arr !. "join") str

slice :: Expr Double -> Expr Double -> Expr a -> Expr a -- Expr (Array (Expr a)) -> Expr (Array (Expr a))
slice a b s = call (s !. "slice") [a, b]

-- ** Pure

length :: Expr [a] -> Expr Int
length as = as !. "length"

last :: Expr [a] -> Expr a
last as = as .! (JS.API.length as - lit 1)

indexOf :: Expr [a] -> Expr a -> Expr Int
indexOf as a = call1 (as !. "indexOf") a

splice :: Expr [a] -> Expr Int -> Expr Int -> Expr [a]
splice as a b = call (as !. "splice") [a, b]

concat :: Expr a -> Expr a -> Expr [a]
concat a b = call1 (b !. "concat") a


-- arrRemove e arr = do  arr !. "indexOf"

-- * Date

data Date

dateObj = ex "new Date"

dateFrom :: Expr a -> Expr Date
dateFrom = call1 dateObj

date :: Expr Date
date = call0 dateObj

now :: Expr Int
now = call0 $ ex "Date" !. "now"

getTime :: Expr Int
getTime = call0 $ ex "Date" !. "getTime"

-- * JSON

jSON :: Expr a
jSON = ex "JSON"

fromJSON :: Expr String -> Expr a
fromJSON = call1 (jSON !. "parse")

toJSON :: Expr a -> Expr String
toJSON = call1 (jSON !. "stringify")

-- * String

toString :: Expr a -> Expr String
toString obj = call0 (obj !. "toString")

split :: Expr String -> Expr String -> Expr [String]
split str sep = call1 (str !. "split") sep

trim :: Expr String -> Expr String
trim s = call0 (s !. "trim")

instance Semigroup (Expr String) where
  a <> b = a + b
instance Monoid (Expr String) where
  mempty = ""

-- * Number

parseFloat a = call1 (ex "parseFloat") a

nearestInt n = call1 (math "round") n

min a b = ternary (a .< b) a b

max a b = ternary (a .> b) a b

floor' = call1 (math "floor")

ceiling' = call1 (math "ceil")

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

-- * Async

data JobId

timeout name f ms = call (ex name) [ Cast f, lit ms]

setInterval :: Expr a -> Int -> Expr JobId
setInterval = timeout "setInterval"

clearInterval :: Expr JobId -> Expr ()
clearInterval id = call1 (ex "clearInterval") id

setTimeout :: Expr a -> Int -> Expr JobId
setTimeout = timeout "setTimeout"

clearTimeout :: Expr JobId -> Expr ()
clearTimeout id = call1 (ex "clearTimeout") id

-- ** Consoleobject/debugging

data Console
console = ex "console" :: Expr Console

consoleLog :: [Expr a] -> M r ()
consoleLog args = bare $ call (console !. "log") args

consoleError :: [Expr a] -> M r ()
consoleError args = bare $ call (ex "console" !. "error") args

log msg = consoleLog [msg]
dir msg = bare $ call1 (console !. "dir") msg

debug var expr = do
   ex var .= expr
   consoleLog [toString expr + lit " (in: \"" + lit var + lit "\")"]
