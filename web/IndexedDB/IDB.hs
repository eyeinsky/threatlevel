module IndexedDB.IDB where

import Prelude2 hiding (Index)
import qualified Data.Text.Lazy as TL
import X hiding (head, get)
import JS
import qualified JS


data DB
data Transaction

data ObjectStore a
data Index
data OI a

data Request

data Key
data KeyRange
data KeyOrRange

data Cursor a
data Direction -- next, nextunique, prev, prevunique; default: next

openDB :: Expr String -> Expr Int -> Expr a -> Expr b
openDB name version upgrade
  = call (ex "idb" !. "openDB") [name, Cast version, Cast upgrade]

open :: Expr String -> Promise DB
open name = openDB name Undefined Undefined

deleteDB :: Expr String -> Promise ()
deleteDB name = call1 (ex "idb" !. "deleteDB") name

-- * DB

close :: Expr DB -> Expr ()
close db = call0 $ db !. "close"

-- Object Store

createObjectStore :: Expr DB -> Expr String -> Expr (ObjectStore a)
createObjectStore db name = call1 (db !. "createObjectStore") name

-- | idb convenience addition
done :: Expr Transaction -> Promise ()
done tx = tx !. "done"

-- | Helper to run a transaction, my own addition.
transaction :: Function f => Expr DB -> [Expr String] -> Expr String -> f -> JS.M () ()
transaction db storeNames mode body = do
  tx :: Expr Transaction <- new $ call (db !. "transaction") [lit storeNames, lit mode]
  let stores = map (objectStore tx) storeNames :: [Expr (ObjectStore a)]
  body' <- async body
  bare $ call (Cast body') (Cast tx : stores)

-- * Transaction

objectStoreNames :: Expr dbOrTx -> Expr [String]
objectStoreNames t = t !. "objectStoreNames"

mode :: Expr Transaction -> Expr String
mode t = t !. "mode"

abort t = call0 (t !. "abort")

objectStore :: Expr Transaction -> Expr String -> Expr (ObjectStore a)
objectStore tx name = call1 (tx !. "objectStore") name


-- * ObjectStore & Index

count0 = c0 "count" :: Expr (OI a) -> Expr Int
count1 = c1 "count" :: Expr KeyOrRange -> Expr (OI a) -> Expr Int

-- | Value or first value if key range
get = c1 "get" :: Expr KeyOrRange -> Expr (OI a) -> Promise a

-- | Returns primary key
getKey = c1 "get" :: Expr KeyOrRange -> Expr (OI a) -> Promise Key

-- | Get all objects in store/index
getAll0 = c0 "getAll" :: Expr (ObjectStore a) -> Promise [a]
getAll1 = c1 "getAll" :: Expr KeyOrRange -> Expr (ObjectStore a) -> Promise [a]
getAll2 = c2 "getAll" :: Expr KeyOrRange -> Expr Int -> Expr (ObjectStore a) -> Promise [a]

-- | Get all objects in store/index
getAllKeys0 = c0 "getAllKeys"
  :: Expr (OI a) -> Promise [Key]
getAllKeys1 = c1 "getAllKeys"
  :: Expr KeyOrRange -> Expr (OI a) -> Promise [Key]
getAllKeys2 = c2 "getAllKeys"
  :: Expr KeyOrRange -> Expr Int -> Expr (OI a) -> Promise [Key]


-- | Cursor over values
openCursor0 = c0 "openCursor"
  :: Expr (OI a) -> Promise (Cursor a)
openCursor1 = c1 "openCursor"
  :: Expr KeyRange -> Expr (OI a) -> Promise (Cursor a)
openCursor2 = c2 "openCursor"
  :: Expr KeyRange -> Expr Direction -> Expr (OI a) -> Promise (Cursor a)

-- | Cursor over keys
openKeyCursor0 = c0 "openKeyCursor"
  :: Expr (OI a) -> Promise (Cursor Key)
openKeyCursor1 = c1 "openKeyCursor"
  :: Expr KeyRange -> Expr (OI a) -> Promise (Cursor Key)
openKeyCursor2 = c2 "openKeyCursor"
  :: Expr KeyRange -> Expr Direction -> Expr (OI a) -> Promise (Cursor Key)


-- * Object Store

-- | Properties: name, keyPath, indexNames, autoIncrement

add1 = c1 "add"
  :: Expr a -> Expr (ObjectStore a) -> Promise Request
add2 = c2 "add" -- | Errors when key already exists
  :: Expr a -> Expr String -> Expr (ObjectStore a) -> Promise Request

put1 = c1 "put" -- | Updates when key already exists
  :: Expr a -> Expr (ObjectStore a) -> Promise Request
put2 = c2 "put"
  :: Expr a -> Expr String -> Expr (ObjectStore a) -> Promise Request

delete = c1 "delete" -- | Delete values with key
  :: Expr KeyOrRange -> Expr (ObjectStore a) -> Promise Request

clear = c0 "clear" :: Expr (ObjectStore a) -> Promise ()

index = c1 "index" -- | Get index with name
  :: Expr String -> Expr (ObjectStore a) -> Promise Index
createIndex2 = c2 "createIndex"
  :: Expr String -> Expr String -> Expr (ObjectStore a) -> Expr ()
createIndex3 = c3 "createIndex"
  :: Expr String -> Expr String -> Expr a -> Expr (ObjectStore a) -> Expr ()
deleteIndex = c1 "deleteIndex"
  :: Expr String -> Expr (ObjectStore a) -> Expr ()

-- * Index

-- | Properties: name, objectStore, keyPath, multiEntry, unique

-- * Cursor

advance = c1 "advance" :: Expr Int -> Expr (Cursor a) -> Promise (Cursor a)
continue0 = c0 "continue" :: Expr (Cursor a) -> Promise (Cursor a)
continue1 = c1 "continue" :: Expr Key -> Expr (Cursor a) -> Promise (Cursor a)

continuePrimaryKey = c2 "continuePrimaryKey"
  :: Expr Key -> Expr Key -> Expr (Cursor a) -> Promise (Cursor a)

update = c1 "update" :: Expr a -> Expr (Cursor a) -> Promise (Cursor a)
delete_ = c0 "delete" :: Expr (Cursor a) -> Promise (Cursor a) -- returns cursor

-- properties
    -- direction
    -- key
    -- primaryKey
    -- value

c0 attr obj = call0 (obj !. attr)
c1 attr arg1 obj = call1 (obj !. attr) arg1
c2 attr arg1 arg2 obj = call (obj !. attr) [arg1, Cast arg2]
c3 attr arg1 arg2 arg3 obj = call (obj !. attr) [arg1, Cast arg2, Cast arg3]
