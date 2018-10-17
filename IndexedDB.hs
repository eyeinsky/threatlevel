module IndexedDB where

import Prelude2 hiding (Index)
import qualified Data.Text.Lazy as TL
import X hiding (head, get)
import JS hiding (String, NumberI)
import qualified JS.Types2 as JS hiding (Array)
import qualified JS as JS0

type Set a = [a]

class Key_ a where
  dbName :: a -> TL.Text
  getKey_ :: a -> TL.Text

-- updates :: Key a => Set a -> js
updates s = do
  db <- async $ do
    retrn $ open "mtriq"
  store <- async $ consoleLog ["hej"] -- todo $ dbName (head s) -- get the object store for this db
  forM_ s $ \o -> do
    -- bare $ put (getKey o) o store
    consoleLog [""]

-- * IndexedDB via idb.js

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
data Direction
-- "next", "nextunique", "prev", and "prevunique". The default is "next".


open' name version upgrade = call (ex "idb" !. "open") [name, version, upgrade]

open :: JS.String -> Promise DB
open name = open' name Undefined Undefined

deleteDb :: JS.String -> Promise ()
deleteDb name = call1 (ex "idb" !. "delete") name

-- * DB

-- attributes:
-- name
-- version
-- objectStoreNames

close :: Expr DB -> Expr ()
close db = call0 $ db !. "close"

transaction :: Expr DB -> Expr Transaction
transaction db = call0 $ db !. "transaction"

-- * UpgradeDB

-- ..

-- * Transaction

-- | properties:
complete :: Expr Transaction -> Promise ()
complete t = t !. "complete"

objectStoreNames t = t !. "objectStoreNames"
mode t = t !. "mode"
abort t = call0 (t !. "abort")

objectStore :: Expr Transaction -> Expr (ObjectStore a)
objectStore t =  t !. "objectStore"


-- * ObjectStore & Index

count0 = c0 "count" :: Expr (OI a) -> Expr JS.NumberI
count1 = c1 "count" :: Expr KeyOrRange -> Expr (OI a) -> Expr JS.NumberI

-- | Value or first value if key range
get = c1 "get" :: Expr KeyOrRange -> Expr (OI a) -> Promise a

-- | Returns primary key
getKey = c1 "get" :: Expr KeyOrRange -> Expr (OI a) -> Promise Key

-- | Get all objects in store/index
getAll0 = c0 "getAll" :: Expr (ObjectStore a) -> Promise (JS.Array a)
getAll1 = c1 "getAll" :: Expr KeyOrRange -> Expr (ObjectStore a) -> Promise (JS.Array a)
getAll2 = c2 "getAll" :: Expr KeyOrRange -> JS.NumberI -> Expr (ObjectStore a) -> Promise (JS.Array a)

-- | Get all objects in store/index
getAllKeys0 = c0 "getAllKeys"
  :: Expr (OI a) -> Promise (JS.Array Key)
getAllKeys1 = c1 "getAllKeys"
  :: Expr KeyOrRange -> Expr (OI a) -> Promise (JS.Array Key)
getAllKeys2 = c2 "getAllKeys"
  :: Expr KeyOrRange -> JS.NumberI -> Expr (OI a) -> Promise (JS.Array Key)


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
  :: Expr a -> JS.String -> Expr (ObjectStore a) -> Promise Request

put1 = c1 "put" -- | Updates when key already exists
  :: Expr a -> Expr (ObjectStore a) -> Promise Request
put2 = c2 "put"
  :: Expr a -> JS.String -> Expr (ObjectStore a) -> Promise Request

delete = c1 "delete" -- | Delete values with key
  :: Expr KeyOrRange -> Expr (ObjectStore a) -> Promise Request

clear = c0 "clear" :: Expr (ObjectStore a) -> Promise ()

index = c1 "index" -- | Get index with name
  :: JS.String -> Expr (ObjectStore a) -> Promise Index
createIndex2 = c2 "createIndex"
  :: JS.String -> JS.String -> Expr (ObjectStore a) -> Expr ()
createIndex3 = c3 "createIndex"
  :: JS.String -> JS.String -> Expr a -> Expr (ObjectStore a) -> Expr ()
deleteIndex = c1 "deleteIndex"
  :: JS.String -> Expr (ObjectStore a) -> Expr ()

-- | idb.js iterateCursor, iterateKeyCursor

-- * Index

-- | Properties: name, objectStore, keyPath, multiEntry, unique
-- | idb.js: iterateCursor, iterateKeyCursor

-- * Cursor

advance = c1 "advance" :: JS.NumberI -> Expr (Cursor a) -> Promise (Cursor a)
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
