module JS_API where

import Prelude2 hiding ((.-))
import JS_Types
import JS_Syntax
import JS_Monad
import JS_Ops_Untyped


-- * Array

push :: Expr a -> Expr (Array (Expr a)) -> M r ()
push a as = bare $ call1 (as !. "push") a

length :: Expr (Array (Expr a)) -> Expr NumberI
length as = as !. "length"

last :: Expr (Array (Expr a)) -> Expr a
last as = as .! (JS_API.length as .- ulit 1)

-- * Date

now = ex "Date" !. "now"

-- * JSON

jSON = ex "JSON"

fromJSON = call1 (jSON !. "parse")
toJSON = call1 (jSON !. "stringify")


-- * Event

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
