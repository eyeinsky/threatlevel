{-# LANGUAGE DeriveAnyClass #-}
module JS.Upstream
  ( (!/), (!//)
  , module JS
  ) where

import Prelude2
import JS hiding (String)
import qualified JS
import JS.Syntax (ToULiteral(..))

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as LL

import Data.Typeable as DT
import Data.Data as DD
import GHC.Generics as DG

import qualified Data.Aeson as A
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Kind

-- import JS.Render
import Render (render, Render)

a !/ b = call0 (a !. b)
a !// b = call1 (a !. b)

-- * js DataConstructor a b

-- ** Examples

f = BL.putStrLn . A.encode

t = do
  f (1 :: Int)      -- 1
  f ()              -- []
  f (1.3 :: Double) -- 1.3
  f ("abc" :: String) -- "abc"

data A0
  = A0
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A10
  = A10 Int
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A7
  = A7 Int Double String
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A1
  = A1A
  | A1B
  | A1C
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A2
  = A2A Int String
  | A2B Double
  | A2C
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A4
  = A4A { a4a :: Int }
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A8
  = A8A { a8a :: Int, a8b :: Double }
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A5
  = A5A { a5a :: Int, a5b :: Double }
  | A5B
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A9
  = A9A { a9a :: Int, a9b :: Double }
  | A9B String
  | A9C String Double
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

data A6
  = A6A { a6x :: Int, a6a :: Int, a6b :: Double }
  | A6B { a6c :: String }
  | A6C
  deriving (DT.Typeable, DD.Data, DG.Generic, Show)

deriving instance A.ToJSON A0
deriving instance A.ToJSON A1
deriving instance A.ToJSON A2
deriving instance A.ToJSON A4
deriving instance A.ToJSON A5
deriving instance A.ToJSON A6
deriving instance A.ToJSON A7
deriving instance A.ToJSON A8
deriving instance A.ToJSON A9
deriving instance A.ToJSON A10
deriving instance A.FromJSON A6

-- * Tests

tests = do
  test A0 (obj A0)

  test (A10 44) (obj A10 (ulit 44))

  test (A7 44 9.9 "hej") (obj A7 (ulit 44) (ulit 9.9) (ulit "hej"))

  test A1A (obj A1A)
  test A1B (obj A1B)
  test A1C (obj A1C)

  test
    (A2A 12 "hejhoo")
    (obj A2A (ulit 12) (ulit "hejhoo"))

  test
    (A2B 88.99)
    (obj A2B (ulit 88.99))
  test
    (A2C)
    (obj A2C)

  test
    (A4A 1)
    (obj A4A (ulit 1))

  test
    (A8A 1 9.799)
    (obj A8A (ulit 1) (ulit 9.799))

  test
    (A5A 1 9.799)
    (obj A5A (ulit 1) (ulit 9.799))
  test
    A5B
    (obj A5B)

  test
    (A9A 1 2.2)
    (obj A9A (ulit 1) (ulit 2.2))
  test
    (A9B "hoo")
    (obj A9B "hoo")
  test
    (A9C "hoo" 9.9)
    (obj A9C "hoo" 9.9)

  test
    (A6A 1 1 2.2)
    (obj A6A (ulit 1) (ulit 1) (ulit 2.2))
  test
    (A6B "hej")
    (obj A6B "hej")

test :: (Render a, A.ToJSON b, Data b, Show b) => b -> a -> IO ()
test orig expr = let
  label = show (toConstr orig)
  (equal, roundtripAeson) = roundtrip orig expr
  rendered = TL.unpack $ render' expr
  in if equal
    then putStrLn $ label <> ": OK" <> " " <> rendered
    else do
    putStrLn ""
    putStrLn $ label <> ": NOT OK !!!"
    putStrLn $ "  aeson:" <> (A.encode orig^.LL.utf8.unpacked)
    putStrLn $ "  render: " <> rendered
    putStrLn $ "  roundtrip aeson:" <> (show roundtripAeson)
    putStrLn $ "  show: " <> show orig
    putStrLn ""


roundtrip :: (Render a1, A.ToJSON a2) => a2 -> a1 -> (Prelude2.Bool, Maybe A.Value)
roundtrip orig expr =
  (equal, roundtripAeson)
  where
    text = render' expr :: TL.Text
    bs = text^.re LL.utf8
    roundtripAeson = A.decode bs :: Maybe A.Value
    equal = maybe False (A.toJSON orig ==) roundtripAeson

render' = render undefined

-- * Generic data type handling in front-end

obj :: forall a. (J a, GetConstr a) => a -> Result a
obj c = case fs of
  _ : _ -> rf fs (maybe mempty pure stub) c
  _ -> case totalFieldCount of
    0 -> case constructorCount of
      0 -> error "error" -- Void
      1 -> ea c
      _ -> str c (tag c)
    _ -> pf [] stub c
  where
    fs = recordFields c :: [String]
    stub :: Maybe (String, Expr ())
    stub = if maxConstrIndex dataType > 1
      then Just ("tag", ulit $ tag c)
      else Nothing

    dataType = constrType constr :: DataType
    (constr, totalFieldCount) = getConstr c :: (Constr, Int)
    constructorCount = Prelude2.length (dataTypeConstrs dataType)



countFields :: Data a => a -> Int
countFields c = Prelude2.length $ gmapQ (const ()) (c) -- $

-- countConstrFields :: forall a. Data (Final a) => Constr -> Const Int a
-- countConstrFields c = Const $ countFields (fromConstr c :: a)

xx = countFields (A6B todo)

-- gmapQ :: Data a => (forall d. Data d => d -> u) -> a -> [u]
-- x = gmapQ (\d -> toConstr d) (Foo 5 'a')



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
  rf (f : fs) acc c = \(a :: Expr a) -> rf fs (acc <> [(f, Cast a)]) (c undefined)
  pf acc mStub c = \(a :: Expr a) -> pf (acc <> [Cast a]) mStub (c undefined)
  ea c = \(a :: Expr a) -> ea (c undefined)
  str c stub = \(a :: Expr a) -> str (c undefined) stub
  numberOfFields c = 1 + numberOfFields (c undefined)
instance {-# OVERLAPPABLE #-} (Result a ~ Expr a) => J a where
  rf _ acc _ = Cast (ulit acc)
  pf arr mStub _ = case arr of
    -- multiple plain fields
    _ : _ : _ -> case mStub of
      Just tag -> ulit $ tag : [("contents", ulit arr)]
      _ -> ulit arr
    -- single plain field
    [e] -> case mStub of
      Just tag -> ulit $ tag : [contents e]
      _ -> Cast e
    -- no fields
    [] -> case mStub of
      Just tagged -> ulit [tagged]
      _ -> emptyObject
    where
      contents v = ("contents", v)
  ea c = emptyArray
  str c t = ulit t
  numberOfFields c = 0

emptyObject = ulit ([] :: [(String, Expr ())]) -- :: Expr a
emptyArray = ulit ([] :: [Expr ()]) -- :: Expr a

-- * GetConstr

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
    f dc = fromConstr dc :: a
    ns = map (countFields . f) dcs
