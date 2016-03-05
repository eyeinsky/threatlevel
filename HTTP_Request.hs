module HTTP_Request where

import Prelude2
-- network, network-simple
import qualified Network.Simple.TCP as NS
import qualified Network.Socket.ByteString as N
import qualified Network.Socket as S

import           HTTP_Common
import qualified HTTP_Header as H
import           HTTP_Netw
import           HTTP_Response
import           URL

-- * Request

-- | The main thing you want to do with a Request.
perform :: BodyAs b => Request b -> IO (ResponseR b)
perform req = let
      u' = url req
      payload = toPayload' req
      tcpto = TCPTo (u' & authority & port) (u' & authority & host)
   in connect tcpto $ \(soc, remoteAddr) -> do
      _ <- send soc payload
      mResp <- NS.recv soc 10000
      return (parseResponse $ fromJust mResp)

data Request b where
   GET     :: Common -> Request b
   POST    :: Common -> Body -> Request b
   HEAD    :: Common         -> Request () -- no body returned
   PUT     :: Common -> Body -> Request b
   DELETE  :: Common         -> Request b  -- no body sent
   -- TRACE   :: Common -> Request b
   -- OPTIONS :: Common -> Request b
   -- CONNECT :: Common -> Request b
   -- PATCH   :: Common -> Request b
   -- CUSTOM..

type Common = (URL, [H.Header])
common req = case req of
   GET  com   -> com
   POST com _ -> com
   HEAD com   -> com
   PUT  com _ -> com
   DELETE com -> com
   -- _ -> error "HTTP.getRequestCommon"

url r = r & common & fst; url :: Request a -> URL

instance ToPayload Common where
   toPayload (url, hdrs)
       = f path url <> f params url <> " " <> http11 <> nl
      <> toPayload hdrs
      where
         http11 = "HTTP/1.1"
         f g x = toPayload $ g x

instance ToPayload (Request b) where
   toPayload req = case req of
      GET    c   -> mk "GET"    c Nothing
      POST   c b -> mk "POST"   c (Just b)
      HEAD   c   -> mk "HEAD"   c Nothing
      PUT    c b -> mk "PUT"    c (Just b)
      DELETE c   -> mk "DELETE" c Nothing
      where
         mk :: T -> Common -> Maybe Body -> T
         mk m c mb = m <> " " <> toPayload c
                  <> maybe "" ((nl2 <>) . toPayload) mb <> nl2
                  -- ^ TODO this nl2 stuff correct?

newtype Body = Body T deriving (Show, Eq)

instance ToPayload Body where
   toPayload (Body t) = t
