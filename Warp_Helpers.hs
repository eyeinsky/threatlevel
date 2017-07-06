module Warp_Helpers
  ( module Warp_Helpers
  , TLS.tlsSettings
  , TLS.TLSSettings
  ) where

import Prelude2 as P
import Data.Function (on)
import Control.Monad (guard)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM

import Control.Concurrent

import Network.Wai -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)
import Network.Wai.Handler.Warp hiding (getPort) -- (Response, Request, queryString, requestHeaders, pathInfo, requestBody)
import qualified Network.Wai.Handler.WarpTLS as TLS
import Network.HTTP.Types

import HTTP.Common
import URL

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
   in do
      traverse_ id $ (forkIO . runHttps) <$> https
      case portSites of
         x : xs -> let f = uncurry runDomains
            in mapM_ (forkIO . f) xs >> f x
         _ -> print "Nothing to run"
   where
      pullPort xs@ (x : _) = (view _2 x, map (view _1) xs)

runHttps :: Https -> IO ()
runHttps ((_, init), (_, auth, bindPort), tls) = do
  handler <- init auth
  TLS.runTLS tls (mkPort bindPort) (\req resp -> resp =<< handler req)
