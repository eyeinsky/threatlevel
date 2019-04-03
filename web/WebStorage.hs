module WebStorage where

import JS
import JS.Types2 as JT

-- * Local and session storage

type Storage = Expr ()

key :: JT.NumberI -> Storage -> JT.String
key n s = call1 (s !. "key") n

getItem :: JT.String -> Storage -> JT.String
getItem k s = call1 (s !. "getItem") k

setItem :: JT.String -> Expr a -> Storage -> JT.String
setItem k v s = call (s !. "setItem") [k, Cast v]

removeItem :: JT.String -> Storage -> Expr ()
removeItem k s = call1 (s !. "removeItem") k

clear :: Storage -> Expr ()
clear s = call0 (s !. "clear")

-- length and key can be used to iterate through all keys
