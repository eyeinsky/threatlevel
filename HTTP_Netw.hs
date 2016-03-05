module HTTP_Netw where

import Prelude2 hiding (unlines, un, (&))
import Data.Word (Word8, Word16)

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as TE
import qualified Data.Text.Encoding as TSE

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as B
import qualified Data.ByteString.Char8 as B8

-- network, network-simple
import qualified Network.Simple.TCP as NS
import qualified Network.Socket.ByteString as N
import qualified Network.Socket as S

-- aeson
import qualified Data.Aeson as JSON

import Text.Format

import HTTP_Common
import URL

-- * Networking

data TCPTo = TCPTo Port Host -- ^ just a shim now

connect :: TCPTo -> ((S.Socket, S.SockAddr) -> IO a) -> IO a
connect (TCPTo port host) f = do
   NS.connect host' port' f
   where host' = toStr host
         port' = show $ unPort port
         toStr = T.unpack . toPayload


newtype Payload = Payload B.ByteString deriving (Show)
-- ^ Something sent over the net

send soc (Payload bs) = NS.send soc bs
toPayload' = Payload . BL.toStrict . TE.encodeUtf8 . toPayload -- TODO propagate BS
