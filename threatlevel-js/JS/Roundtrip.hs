{-# LANGUAGE DeriveAnyClass #-}
module JS.Roundtrip
  ( Data, Generic, Typeable
  , module JS
  , obj
  ) where

import Data.List

import Data.Data
import GHC.Generics

import Common.Prelude
import JS hiding (String)
import JS.TH

-- * Generic data type handling in front-end

obj :: forall a. (J a, GetConstr a) => a -> Result a
obj dc = case fieldNames of
  -- | Multi data-constructor
  _ : _ -> rf fieldNames (maybe mempty pure stub) dc

  -- | Single data-constructor
  _ -> case totalFieldCount of
    0 -> case length (dataTypeConstrs dataType) of
      0 -> error "error" -- Void
      1 -> ea dc
      _ -> str dc (tag dc)
    _ -> pf [] stub dc
  where
    (constr, totalFieldCount) = getConstr dc :: (Constr, Int)
    dataType = constrType constr :: DataType

    prefix = lowerFirst $ show constr
    fieldNames = map (\field -> maybe field lowerFirst $ stripPrefix prefix field) $ recordFields dc :: [String]

    stub :: Maybe (String, Expr ())
    stub = if maxConstrIndex dataType > 1
      then Just ("tag", lit $ tag dc)
      else Nothing

countFields :: Data a => a -> Int
countFields a = length $ gmapQ (\_ -> ()) a


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

emptyObject :: Expr b
emptyObject = lit ([] :: [(String, Expr ())]) -- :: Expr a

emptyArray :: Expr b
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
