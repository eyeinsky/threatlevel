module Warp_Helpers
  ( module Warp_Helpers
  , TLS.tlsSettings
  , TLS.TLSSettings
  ) where

import Pr
import Data.Function (on)
import Control.Monad (guard)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM

import Control.Concurrent
import qualified Control.Concurrent.Async as Async

import Network.Wai
import Network.Wai.Handler.Warp hiding (getPort)
import qualified Network.Wai.Handler.WarpTLS as TLS
import Network.HTTP.Types

import HTTP.Common hiding (un)
import URL.ToPayload as URL

getRequestBody :: Request -> IO BL.ByteString
getRequestBody req = BL.fromChunks <$> loop
   where
      loop = requestBody req >>= re
      re chunk = B.null chunk ? return [""] $ (chunk:) <$> loop

myRun :: URL.Port -> Handler -> IO ()
myRun (URL.Port port) f
   = runSettings (setPort (fromIntegral port) $ defaultSettings) $ \ r r' -> r' =<< f r

-- * Types

type Handler = Request -> IO Response
type Site = (Authority, IO Handler)

type AppName = String

type AppDef = (AppName, Authority -> IO Handler)
type Rule = (AppName, Authority, URL.Port)
type Https =  (AppDef, Rule, TLS.TLSSettings)

data Server = Server (Maybe Https) [AppDef] [Rule]

-- * Run http domains

runDomains :: URL.Port -> [Site] -> IO ()
runDomains bindPort sites = do
  pairs' <- initSites sites
  runSettings
    (mkPort bindPort)
    (\req respond -> let
        domain = fromJust $ getDomain req :: B.ByteString
        resp = snd <$> find ((== domain) . fst) pairs' :: Maybe Handler
      in respond =<< fromMaybe noDomain (($ req) <$> resp)
    )

mkPort port = setPort (fromIntegral $ port^.URL.un) $ defaultSettings

-- ** Helpers

initSites :: [Site] -> IO [(B.ByteString, Handler)]
initSites sites = forM sites initSite

initSite :: (Authority, IO Handler) -> IO (B.ByteString, Handler)
initSite (a, b) = (toHost a,) <$> b

toHost :: Authority -> B.ByteString
toHost authority = BL.toStrict . TLE.encodeUtf8 $ withoutSchema baseUrl
   where
     baseUrl = BaseURL (Proto "http") (view host authority) (view port authority)

noDomain :: Monad m => m Response
noDomain = return $ responseLBS status404 [] "No host"

getDomain :: Request -> Maybe B.ByteString
getDomain = lookup "Host" . requestHeaders

-- * Run web server

runServer :: Server -> IO ()
runServer (Server https defs rules) = let
      join :: [(URL.Port, [Site])]
      join = do
         (name, app) <- defs
         (name', autho, bindPort) <- rules
         guard (name == name')
         return (bindPort, [(autho, app autho)])
      portSites :: [(URL.Port, [Site])]
      portSites = join & HM.fromListWith (<>) & HM.toList

      maybeHttpsIO = runHttps <$> https :: Maybe (IO ())
      httpIO = map (uncurry runDomains) portSites :: [IO ()]
      li = maybe httpIO (:httpIO) maybeHttpsIO :: [IO ()]
   in Async.mapConcurrently_ id li

runHttps :: Https -> IO ()
runHttps ((_, init), (_, auth, bindPort), tls) = do
  handler <- init auth
  TLS.runTLS tls (mkPort bindPort) (\req resp -> resp =<< handler req)
