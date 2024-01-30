module Server.Sessions where

import Common.Prelude hiding (map)
import Data.Text qualified as TS
import Data.HashMap.Strict qualified as HM
import Data.Word (Word64)
import Numeric
import Control.Concurrent.MVar
import System.Random as Random
import System.Random.Stateful as Random

-- * API

type Id = TS.Text
data Store a = Store
  { storeMap :: MVar (HM.HashMap Id a)
  , storeGen :: IOGenM StdGen
  }

createStore :: IO (Store a)
createStore = Store <$> newMVar mempty <*> (newIOGenM =<< Random.initStdGen)

update :: Id -> a -> Store a -> IO ()
update id value store = modifyMVar_ (storeMap store) $ pure . HM.insert id value

new :: a -> Store a -> IO Id
new value store = do
  id <- genId $ storeGen store
  update id value store
  return id

get :: Id -> Store a -> IO (Maybe a)
get id store = readMVar (storeMap store) <&> HM.lookup id

-- * Helpers

genId :: IOGenM StdGen -> IO TS.Text
genId gen = TS.concat . fmap sh <$> replicateM 4 (uniformM gen)
  where sh (bytes :: Word64) = TS.pack $ showHex bytes ""

listTokens :: Store a -> IO [Id]
listTokens store = readMVar (storeMap store) <&> HM.keys
