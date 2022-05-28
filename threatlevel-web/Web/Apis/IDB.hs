module Web.Apis.IDB where
-- | Depends on NPMs idb package being present

import Data.Text qualified as TS
import Common.Prelude hiding (Index)
import JS hiding (c1, c2, c3) -- TODO


data DB
data Transaction
data Mode
-- = Readonly | ReadWrite | VersionChange

data ObjectStore a
data Index
data OI a

data Request

data Key
data KeyRange
data KeyOrRange

data Cursor a
data Direction -- next, nextunique, prev, prevunique; default: next

-- * DB

openDB :: Expr String -> Expr Int -> Expr a -> Expr b
openDB name version upgrade
  = call (ex "idb" !. "openDB") [name, Cast version, Cast upgrade]

open :: Expr String -> Promise DB
open name = openDB name Undefined Undefined

deleteDB :: Expr String -> Promise ()
deleteDB name = call1 (ex "idb" !. "deleteDB") name

close :: Expr DB -> Expr ()
close db = call0 $ db !. "close"

-- * Object Store

createObjectStore :: Expr String -> Expr o -> Expr DB -> Expr (ObjectStore a)
createObjectStore name opts db = call (db !. "createObjectStore")
  [Cast name, opts]

-- * Transaction

-- | Helper to run a transaction, my own addition. Body gets the
-- transaction and object stores as arguments, and what it returns is
-- returned as a promise from this helper.
transaction
  :: (JS m, ToExpr a1, Function f m) =>
     Expr a2 -> [Expr String] -> a1 -> f -> m (Expr c)
transaction db storeNames mode body = do
  tx :: Expr Transaction <- const $ call (db !. "transaction") [lit storeNames, lit mode]
  let stores = map (objectStore tx) storeNames :: [Expr (ObjectStore a)]
  body' <- async body
  pure $ call (Cast body') (Cast tx : stores)

ro, rw, vc :: Expr Mode
ro = "readonly"
rw = "readwrite"
vc = "versionchange"

-- | idb convenience addition
done :: Expr Transaction -> Promise ()
done tx = tx !. "done"

mode :: Expr Transaction -> Expr Mode
mode t = t !. "mode"

abort :: Expr Transaction -> Expr ()
abort t = call0 (t !. "abort")





objectStoreNames :: Expr dbOrTx -> Expr [String]
objectStoreNames t = t !. "objectStoreNames"

objectStore :: Expr Transaction -> Expr String -> Expr (ObjectStore a)
objectStore tx name = call1 (tx !. "objectStore") name

-- * ObjectStore & Index

count0 :: Expr (OI a) -> Expr Int
count0 = c0 "count"

count1 :: Expr KeyOrRange -> Expr (OI a) -> Expr Int
count1 = c1 "count"

-- | Value or first value if key range
get :: Expr KeyOrRange -> Expr (OI a) -> Promise a
get = c1 "get"

-- | Returns primary key
getKey :: Expr KeyOrRange -> Expr (OI a) -> Promise Key
getKey = c1 "get"

-- | Get all objects in store/index
getAll0 :: Expr (ObjectStore a) -> Promise [a]
getAll0 = c0 "getAll"
getAll1 :: Expr KeyOrRange -> Expr (ObjectStore a) -> Promise [a]
getAll1 = c1 "getAll"
getAll2 :: Expr KeyOrRange -> Expr Int -> Expr (ObjectStore a) -> Promise [a]
getAll2 = c2 "getAll"

-- | Get all objects in store/index
getAllKeys0 :: Expr (OI a) -> Promise [Key]
getAllKeys0 = c0 "getAllKeys"
getAllKeys1 :: Expr KeyOrRange -> Expr (OI a) -> Promise [Key]
getAllKeys1 = c1 "getAllKeys"
getAllKeys2 :: Expr KeyOrRange -> Expr Int -> Expr (OI a) -> Promise [Key]
getAllKeys2 = c2 "getAllKeys"


-- | Cursor over values
openCursor0 :: Expr (OI a) -> Promise (Cursor a)
openCursor0 = c0 "openCursor"
openCursor1 :: Expr KeyRange -> Expr (OI a) -> Promise (Cursor a)
openCursor1 = c1 "openCursor"
openCursor2 :: Expr KeyRange -> Expr Direction -> Expr (OI a) -> Promise (Cursor a)
openCursor2 = c2 "openCursor"

-- | Cursor over keys
openKeyCursor0 :: Expr (OI a) -> Promise (Cursor Key)
openKeyCursor0 = c0 "openKeyCursor"
openKeyCursor1 :: Expr KeyRange -> Expr (OI a) -> Promise (Cursor Key)
openKeyCursor1 = c1 "openKeyCursor"
openKeyCursor2 :: Expr KeyRange -> Expr Direction -> Expr (OI a) -> Promise (Cursor Key)
openKeyCursor2 = c2 "openKeyCursor"

-- * Key range

keyRange :: Expr a
keyRange = ex "IDBKeyRange"

upperBound, lowerBound :: Expr a -> Expr Bool -> Expr KeyRange
upperBound key open = call (keyRange !. "upperBound") [key, Cast open]
lowerBound key open = call (keyRange !. "lowerBound") [key, Cast open]

bound :: Expr a -> Expr a -> Expr Bool -> Expr Bool -> Expr KeyRange
bound start end sopen eopen = call (keyRange !- "bound") [start, end, Cast sopen, Cast eopen]

-- * Object Store

-- | Properties: name, keyPath, indexNames, autoIncrement

add :: Expr a -> Expr (ObjectStore a) -> Promise Request
add = c1 "add"

-- | Errors when key already exists
addWithKey :: Expr a -> Expr String -> Expr (ObjectStore a) -> Promise Request
addWithKey = c2 "add"

-- | Updates when key already exists
put :: Expr a -> Expr (ObjectStore a) -> Promise Request
put = c1 "put"

putWithKey :: Expr a -> Expr String -> Expr (ObjectStore a) -> Promise Request
putWithKey = c2 "put"

-- | Delete values with key
delete :: Expr KeyOrRange -> Expr (ObjectStore a) -> Promise Request
delete = c1 "delete"

clear :: Expr (ObjectStore a) -> Promise ()
clear = c0 "clear"

-- * Indexes

-- | Get index with name
index :: Expr String -> Expr (ObjectStore a) -> Promise Index
index = c1 "index"

-- | index name -> key path -> object store -> ...
createIndex :: Expr String -> Expr path -> Expr (ObjectStore a) -> Expr ()
createIndex = c2 "createIndex"

createIndex3 :: Expr String -> Expr String -> Expr a -> Expr (ObjectStore a) -> Expr ()
createIndex3 = c3 "createIndex"

deleteIndex :: Expr String -> Expr (ObjectStore a) -> Expr ()
deleteIndex = c1 "deleteIndex"

-- * Index

-- | Properties: name, objectStore, keyPath, multiEntry, unique

-- * Cursor

advance :: Expr Int -> Expr (Cursor a) -> Promise (Cursor a)
advance = c1 "advance"

continue0 :: Expr (Cursor a) -> Promise (Cursor a)
continue0 = c0 "continue"

continue1 :: Expr Key -> Expr (Cursor a) -> Promise (Cursor a)
continue1 = c1 "continue"

continuePrimaryKey :: Expr Key -> Expr Key -> Expr (Cursor a) -> Promise (Cursor a)
continuePrimaryKey = c2 "continuePrimaryKey"

update :: Expr a -> Expr (Cursor a) -> Promise (Cursor a)
update = c1 "update"

delete_ :: Expr (Cursor a) -> Promise (Cursor a) -- returns cursor
delete_ = c0 "delete"

-- properties
    -- direction
    -- key
    -- primaryKey
    -- value

c0 :: TS.Text -> Expr a -> Expr c
c0 attr obj = call0 (obj !. attr)

c1 :: TS.Text -> Expr b -> Expr a -> Expr c
c1 attr arg1 obj = call1 (obj !. attr) arg1

c2 :: TS.Text -> Expr b -> Expr a1 -> Expr a2 -> Expr c
c2 attr arg1 arg2 obj = call (obj !. attr) [arg1, Cast arg2]

c3 :: TS.Text -> Expr b -> Expr a1 -> Expr a2 -> Expr a3 -> Expr c
c3 attr arg1 arg2 arg3 obj = call (obj !. attr) [arg1, Cast arg2, Cast arg3]
