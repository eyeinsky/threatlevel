module Sessions where

import qualified Data.Text as TS
import qualified Data.Text.Strict.Lens as TS
import qualified Data.Text.Encoding as TS
import qualified Data.HashMap.Strict as HM
import Data.Word (Word64)
import Numeric
import Control.Concurrent.MVar
import qualified System.Random.MWC as MWC
import Prelude2 hiding (map)

-- * API

type Id = TS.Text
data Store a = Store
  { storeMap :: MVar (HM.HashMap Id a)
  , storeGen :: MWC.GenIO
  }
makeFields ''Store

createStore :: IO (Store a)
createStore = Store <$> newMVar mempty <*> MWC.createSystemRandom

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

genId :: MWC.GenIO -> IO Id
genId gen = TS.concat . fmap sh <$> replicateM 4 (MWC.uniform gen)
  where sh (bytes :: Word64) = TS.pack $ showHex bytes ""

hot :: IO Id
hot = do
  s <- createStore
  genId $ s^.gen
