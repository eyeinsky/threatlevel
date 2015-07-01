module JS_API where

import Prelude2 hiding ((.-))
import JS_Types
import qualified JS_Types as T
import JS_Syntax
import JS_Monad
import JS_Ops_Untyped

import Web_HTML

-- * Array

push :: Expr a -> Expr (Array (Expr a)) -> Expr c -- M r ()
push a as = call1 (as !. "push") a

length :: Expr (Array (Expr a)) -> Expr NumberI
length as = as !. "length"

last :: Expr (Array (Expr a)) -> Expr a
last as = as .! (JS_API.length as .- ulit 1)

-- arrRemove e arr = do  arr !. "indexOf"

-- * Date

now = ex "Date" !. "now"

-- * JSON

jSON = ex "JSON"

-- fromJSON :: Expr a -> Expr JT.String
fromJSON = call1 (jSON !. "parse")

toJSON :: Expr a -> Expr T.String
toJSON = call1 (jSON !. "stringify")

-- * String

toString :: Expr a -> Expr T.String
toString obj = call0 (obj !. "toString")

split :: Expr T.String -> Expr T.String -> Expr (T.Array T.String)
split str sep = call1 (str !. "split") sep 

-- * Number

parseFloat a = call1 (ex "parseFloat") a

nearestInt n = call1 (ex "Math" !. "round") n

min a b = ternary (a .< b) a b

max a b = ternary (a .> b) a b

-- * DOM/Event

-- focus :: Expr Tag -> Expr M r ()
focus e = call0 (e !. "focus")

-- blur :: Expr Tag -> M r ()
blur e = call0 (e !. "blur")



-- | Get char from keyboard event
eventKey event = do -- from: http://unixpapa.com/js/key.html
   retrn $ let
         which = event !. "which" -- :: Expr J.Number
         from arg = call (ex "String" !. "fromCharCode") [ arg ]
         -- from which or keyCode
      in ternary (which .== ex "null")
      (from $ event !. "keyCode" ) -- old IE
      (ternary
         (  (which .!= ulit 0 :: Expr JS_Types.Bool)
        .&& event !. "charCode" .!= ulit 0
        ) (from which {-all others-}) Null)

-- * Async

data JobId

setInterval :: Expr a -> Expr b -> M r (Expr JobId)
setInterval f t = new $ call (ex "setInterval") [ Cast f, t]

clearInterval :: Expr JobId -> M r ()
clearInterval id = bare $ call1 (ex "clearInterval") id

setTimeout :: Expr a -> Expr b -> M r (Expr JobId)
setTimeout f t = new $ call (ex "setTimeout") [ Cast f, t]

clearTimeout :: Expr JobId -> M r ()
clearTimeout id = bare $ call1 (ex "clearTimeout") id


-- * XMLHttpRequest (Ajax)

-- Expr URL -> data -> (\ x -> M y z) -> M a b
-- doPost' a b c = call ajaxExpr ["post", a, b, c]
doPost' a b c = do
   aj <- newf $ ajaxExpr
   bare $ call aj [ulit "POST", a, b, c]
doGet' a b c = do
   aj <- newf $ ajaxExpr
   bare $ call aj [ulit "GET", a, b, c]

ajaxExpr meth uri data_ callback = do
   xhr <- new $ ex "new XMLHttpRequest()"
   ifonly (callback .!== Undefined) $ do
      wrap <- newf $ \(ret :: Expr ()) -> do
         text <- new $ xhr !. "responseText"
         json <- new $ fromJSON text
         bare $ call1 callback json
      xhr !. "onload" .= Cast wrap
   bare (call (xhr !. "open") [meth, uri, ulit True]) 
   bare $ call1 (xhr !. "send") data_



-- * Modal dialogs

alert x = bare $ call1 (ex "alert") x

-- prompt

-- ?



-- ** Consoleobject/debugging

consoleLog args = bare $ call (ex "console" !. "log") args

debug var expr = do
   ex var .= expr
   consoleLog [toString expr .+ ulit " (in: \"" .+ ulit var .+ ulit "\")"]
