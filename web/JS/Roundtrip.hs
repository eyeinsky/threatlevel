{-# LANGUAGE DeriveAnyClass #-}
module JS.Roundtrip
  ( Data, Generic, Typeable
  , module JS
  , obj
  ) where

import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as LL
import qualified Data.Aeson as A

import Data.Data
import GHC.Generics

import X.Prelude
import JS hiding (String, length)
import Render (render, Render)

import Debug.Trace

-- * Generic data type handling in front-end

obj :: forall a. (J a, GetConstr a) => a -> Result a
obj c = case fieldNames of
  _ : _ -> rf fieldNames (maybe mempty pure stub) c
  _ -> case totalFieldCount of
    0 -> case constructorCount of
      0 -> error "error" -- Void
      1 -> ea c
      _ -> str c (tag c)
    _ -> pf [] stub c
  where
    fieldNames = dropTypeName name $ recordFields c :: [String]
    stub :: Maybe (String, Expr ())
    stub = if maxConstrIndex dataType > 1
      then Just ("tag", lit $ tag c)
      else Nothing

    dataType = constrType constr :: DataType
    name = dataTypeName dataType

    (constr, totalFieldCount) = getConstr c :: (Constr, Int)
    constructorCount = X.Prelude.length (dataTypeConstrs dataType)

countFields :: Data a => a -> Int
countFields c = X.Prelude.length $ gmapQ (const ()) (c) -- $

dropTypeName :: String -> [String] -> [String]
dropTypeName name fields = map go fields
  where
    lowerFirst (s : tring) = toLower s : tring
    name' = lowerFirst name
    length' = length name'
    go field
      | name' `isPrefixOf` field = lowerFirst $ drop length' field
      | otherwise = field



type family Result a :: *
  where Result (a -> b) = Expr a -> Result b
        Result a = Expr a

type TagName = String
type Stub = [(String, Expr ())]
type MStub = Maybe (String, Expr ())
class J c where
  rf :: [String] -> Stub -> c -> Result c
  pf :: [Expr ()] -> MStub -> c -> Result c
  ea :: c -> Result c
  str :: c -> TagName -> Result c
  numberOfFields :: c -> Int
instance (J b) => J (a -> b) where
  rf fs' acc c = case fs' of
    (f : fs) -> \(a :: Expr a) -> rf fs (acc <> [(f, Cast a)]) (c undefined)
    _ -> todo
  pf acc mStub c = \(a :: Expr a) -> pf (acc <> [Cast a]) mStub (c undefined)
  ea c = \(_ :: Expr a) -> ea (c undefined)
  str c stub = \(_ :: Expr a) -> str (c undefined) stub
  numberOfFields c = 1 + numberOfFields (c undefined)
instance {-# OVERLAPPABLE #-} (Result a ~ Expr a) => J a where
  rf _ acc _ = Cast (lit acc)
  pf arr mStub _ = case arr of
    -- multiple plain fields
    _ : _ : _ -> case mStub of
      Just tag -> lit $ tag : [("contents", lit arr)]
      _ -> lit arr
    -- single plain field
    [e] -> case mStub of
      Just tag -> lit $ tag : [contents e]
      _ -> Cast e
    -- no fields
    [] -> case mStub of
      Just tagged -> lit [tagged]
      _ -> emptyObject
    where
      contents v = ("contents", v)
  ea _ = emptyArray
  str _ t = lit t
  numberOfFields _ = 0

emptyObject = lit ([] :: [(String, Expr ())]) -- :: Expr a
emptyArray = lit ([] :: [Expr ()]) -- :: Expr a

-- * GetConstr

-- | Get the data constructor and total field count.
--
-- TODO: this should be refactored to separate concerns.
class GetConstr a where
  getConstr :: a -> (Constr, Int)
instance {-# OVERLAPPING #-} (GetConstr b) => GetConstr (a -> b) where
  getConstr f = getConstr (f undefined)
instance {-# OVERLAPPABLE #-} (Data a) => GetConstr a where
  getConstr a = (toConstr a, countAllFields a)

tag :: GetConstr a => a -> String
tag c = showConstr (fst $ getConstr c)

recordFields :: GetConstr a => a -> [String]
recordFields c = constrFields (fst $ getConstr c)

countAllFields :: forall a. Data a => a -> Int
countAllFields dc = sum ns
  where
    dt = dataTypeOf dc :: DataType
    dcs = dataTypeConstrs dt :: [Constr]
    ns = map ((countFields :: a -> Int) . fromConstr) dcs
