module HTTP.Request where

import Prelude2
-- network, network-simple
-- import qualified Network.Simple.TCP as NS
-- import qualified Network.Socket.ByteString as N
-- import qualified Network.Socket as S

-- import           HTTP.Common
-- import qualified HTTP.Header as H
-- import           HTTP.Netw
-- import           HTTP.Response
-- import           URL

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as TL
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.Wai
import Network.HTTP.Types

-- * Wai Request

queryText :: Request -> QueryText
queryText = queryString ^ queryToQueryText

-- textFormSubmission :: Request -> IO [(Maybe TL.Text, Maybe (Maybe TL.Text))]
formSubmission :: Request -> IO [(BL.ByteString, Maybe BL.ByteString)]
formSubmission req = do
  lb <- strictRequestBody req
  let pairs = BL8.split '&' lb :: [BL.ByteString] -- queryString
      f (k, v) = case v of
        "" -> (k, Nothing)
        _ -> (k, Just $ BL8.tail v)
      g = strict %~ urlDecode True
      decode (k, v) = (g k, g <$> v)
  return $ map (decode . f . BL8.break (== '=')) pairs

textFormSubmission :: Request -> IO [(TL.Text, Maybe TL.Text)]
textFormSubmission req = do
  formSubmission req <&> map (f *** fmap f)
  where
    f = view TL.utf8

-- * Request

-- | The main thing you want to do with a Request.
-- perform :: BodyAs b => Request b -> IO (ResponseR b)
-- perform req = let
--       u' = url req
--       payload = toPayload' req
--       tcpto = TCPTo (u' & authority & port) (u' & authority & host)
--    in connect tcpto $ \(soc, remoteAddr) -> do
--       _ <- send soc payload
--       mResp <- NS.recv soc 10000
--       return (parseResponse $ fromJust mResp)

-- data Request b where
--    GET     :: Common -> Request b
--    POST    :: Common -> Body -> Request b
--    HEAD    :: Common         -> Request () -- no body returned
--    PUT     :: Common -> Body -> Request b
--    DELETE  :: Common         -> Request b  -- no body sent
   -- TRACE   :: Common -> Request b
   -- OPTIONS :: Common -> Request b
   -- CONNECT :: Common -> Request b
   -- PATCH   :: Common -> Request b
   -- CUSTOM..

-- type Common = (URL, [H.Header])
-- common req = case req of
--    GET  com   -> com
--    POST com _ -> com
--    HEAD com   -> com
--    PUT  com _ -> com
--    DELETE com -> com
   -- _ -> error "HTTP.getRequestCommon"

-- url r = r & common & fst; url :: Request a -> URL

-- instance ToPayload Common where
--    toPayload (url, hdrs)
--        = f path url <> f params url <> " " <> http11 <> nl
--       <> toPayload hdrs
--       where
--          http11 = "HTTP/1.1"
--          f g x = toPayload $ g x

-- instance ToPayload (Request b) where
--    toPayload req = case req of
--       GET    c   -> mk "GET"    c Nothing
--       POST   c b -> mk "POST"   c (Just b)
--       HEAD   c   -> mk "HEAD"   c Nothing
--       PUT    c b -> mk "PUT"    c (Just b)
--       DELETE c   -> mk "DELETE" c Nothing
--       where
--          mk :: T -> Common -> Maybe Body -> T
--          mk m c mb = m <> " " <> toPayload c
--                   <> maybe "" ((nl2 <>) . toPayload) mb <> nl2
--                   -- ^ TODO this nl2 stuff correct?

-- newtype Body = Body T deriving (Show, Eq)

-- instance ToPayload Body where
--    toPayload (Body t) = t
