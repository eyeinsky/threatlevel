module Server.Sessions where

import Common.Prelude hiding (map)
import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HM
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
makeFields ''Store

createStore :: IO (Store a)
createStore = Store <$> newMVar mempty <*> (newIOGenM =<< Random.initStdGen)

update :: Id -> a -> Store a -> IO ()
update id value store = modifyMVar_ (store^.map) $ pure . HM.insert id value

new :: a -> Store a -> IO Id
new value store = do
  id <- genId $ store^.gen
  update id value store
  return id

get :: Id -> Store a -> IO (Maybe a)
get id store = readMVar (store^.map) <&> HM.lookup id

-- * Helpers

genId :: IOGenM StdGen -> IO TS.Text
genId gen = TS.concat . fmap sh <$> replicateM 4 (uniformM gen)
  where sh (bytes :: Word64) = TS.pack $ showHex bytes ""

listTokens :: Store a -> IO [Id]
listTokens store = readMVar (store^.map) <&> HM.keys
