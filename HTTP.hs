{-
TODO
   * implement POST-ing, postForm
   * convert to lens
   * Payload is now BS, but ToPayload renders to T (lazy text)
      - need to look what headers are (ASCII)
      - body can definitely be BS
-}
module HTTP where

import Prelude2 hiding (unlines)
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
import Control.Lens hiding (un, (&))

import Text.Format

import HTTP_Common
import           HTTP_Header (hdr)
import qualified HTTP_Header as H
import qualified HTTP_Header as Hdr
import           HTTP_URL


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


-- * Request

-- | The main thing you want to do with a Request.
perform :: BodyAs b => Request b -> IO (Response b)
perform req = let
      u' = url req
      payload = toPayload' req
      tcpto = TCPTo (u' & port) (u' & host)
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
   


-- * Response

data Response b where
   Response :: BodyAs b => StatusLine -> [H.Header] -> b -> Response b

type StatusLine = B.ByteString

parseResponse :: BodyAs b => B.ByteString -> Response b
parseResponse bs = Response statusLine headers body
   where (statusLine, rest) = B.breakSubstring crlf bs
         (hdrsBs, bodyBs)   = B.breakSubstring (crlf<>crlf) rest
         headers = parseHeaders hdrsBs
         body    = parseBody bodyBs
   -- ^ TODO: implement proper parsing


-- ** Parse response headers

parseHeaders :: B.ByteString -> [H.Header]
parseHeaders bs = pairs
   where rows = tokenise crlf bs :: [B.ByteString]
         br = B8.break (== ':')
         g = TE.decodeUtf8 . BL.fromStrict
         f (b1,b2) = hdr (Hdr.Custom (g b1)) (g b2)
         pairs = map (f . br) rows
         
         tokenise x y = h : if B.null t
               then []
               else tokenise x (B.drop (B.length x) t)
            where (h,t) = B.breakSubstring x y

-- ** Parsing various types of response body

class BodyAs b where
   parseBody :: B.ByteString {-^ defined by NS.recv-} -> b
{- ^ We use BodyAs as a class to be able to extende it to
     arbitrary types. -}

instance BodyAs B.ByteString where
   parseBody = id
instance BodyAs BL.ByteString where
   parseBody = BL.fromStrict
instance BodyAs T.Text where
   parseBody = TE.decodeUtf8 . BL.fromStrict
   -- ^ WARNING: this is unsafe, TODO

instance BodyAs (Maybe JSON.Object) where
   parseBody bs = JSON.decode (BL.fromStrict bs)


-- * Instances 

deriving instance Show b => Show (Response b)
deriving instance Show (Request b)
deriving instance Show URL
deriving instance Show Proto
deriving instance Show Host
deriving instance Show Port
deriving instance Show Path
deriving instance Show Params
deriving instance Show Fragment

deriving instance Eq (Request b)
deriving instance Eq URL
deriving instance Eq Proto
deriving instance Eq Host
deriving instance Eq Port
deriving instance Eq Path
deriving instance Eq Params
deriving instance Eq Fragment



-- * Convenience

bareDomain dom = URL
   (Proto "http") (Domain dom) (Port 80)
   (Path []) (Params []) (Fragment "")
bareIp a b c d = URL
   (Proto "http") (IP4 a b c d) (Port 80)
   (Path []) (Params []) (Fragment "")
hdrsFrom url = z <> (maybe [] (\x -> [hdr Hdr.Host x]) hst) :: [Hdr.Header]
   where
      z = [ hdr Hdr.Accept H.accHtml ]
      hst = case host url of
         Domain d    -> Just d
         IP4 _ _ _ _ -> Nothing

get :: T.Text -> Request a  
get urlT = GET (url, hdrsFrom url)
   where url = bareDomain urlT

getIp a b c d = GET (url, hdrsFrom url)
   where url = bareIp a b c d

post :: URL -> Params -> Request a
post = u
