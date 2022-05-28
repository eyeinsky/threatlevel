module Web.Apis.WebStorage where

import Common.Prelude
import JS

-- * Local and session storage

type Storage = Expr ()

key :: Expr String -> Storage -> Expr String
key n s = call1 (s !. "key") n

getItem :: Expr String -> Storage -> Expr String
getItem k s = call1 (s !. "getItem") k

setItem :: Expr String -> Expr a -> Storage -> Expr String
setItem k v s = call (s !. "setItem") [k, Cast v]

removeItem :: Expr String -> Storage -> Expr ()
removeItem k s = call1 (s !. "removeItem") k

clear :: Storage -> Expr ()
clear s = call0 (s !. "clear")

-- length and key can be used to iterate through all keys
