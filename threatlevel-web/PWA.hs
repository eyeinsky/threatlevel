module PWA where

import qualified Data.Text.Lazy as TL

import X as DOM hiding (url, put)
import qualified JS.BuiltIns.Full as JS


-- * Web Worker

data Worker

self :: Expr a
self = ex "self"

-- ** Internal

-- | Receive dato on 'message' event, apply the provided function and
-- use 'postMessage' to send the result back
pipe :: Function f => f -> M r ()
pipe f = do
  f' <- newf f
  wrap <- newf $ \msg -> send self (call1 f' msg)
  bare $ addEventListener self DOM.Message wrap

-- ** External

createWorker :: URL -> Expr Worker
createWorker path = call1 (ex "new Worker") (lit $ renderURL path)

postMessage :: Expr a -> Expr b -> Expr ()
postMessage obj msg = call1 (obj !. "postMessage") msg

send :: Expr a -> Expr b -> M r ()
send o m = bare $ postMessage o m

receive :: Expr a -> Expr f -> M r ()
receive worker handler = do
  bare $ DOM.addEventListener (Cast worker) DOM.Message handler

-- * Caches API

-- ** CacheStorage

data Caches

caches :: Expr Caches
caches = ex "caches"

open :: Expr String -> Expr Caches -> Promise Cache
open name caches = call1 (caches !. "open" ) name

keys :: Expr caches -> Promise [Request]
keys caches = call0 (caches !. "keys")

-- ** Cache

data Cache

match :: Expr Request -> Expr Cache -> Promise Response
match req cache = call1 (cache !. "match") req

put :: Expr Request -> Expr Response -> Expr Cache -> Promise ()
put req resp cache = call (cache !. "put") [req, Cast resp]

delete :: Expr Request -> Expr Cache -> Promise Bool
delete req cache = call1 (cache !. "delete") req

-- * Fetch API

request :: Expr DOM.ServiceWorkerEvent -> Expr Request
request fetchEvent = fetchEvent !. "request"

clone :: Expr Response -> Expr Response
clone req = call0 (req !. "clone")

url :: Expr Request -> Expr URL
url req = req !. "url"

anyPrefix :: [URL] -> Expr URL -> Expr Bool
anyPrefix patUrls reqUrl = reqUrl !// "match" $ regex (TL.toStrict pat) "i"
  where
    pat = map renderURL patUrls & TL.intercalate "|" & par & (<> "\\b")

-- * Service Worker

-- | ExtendableEvent method, available in service workers
waitUntil :: Event e => Promise () -> Expr e -> Promise ()
waitUntil promise installEvent = call1 (installEvent !. "waitUntil") promise

-- ** Register

register :: URL -> M r ()
register url = let
  cond = "serviceWorker" `In` ex "navigator"
  urlStr = lit $ renderURL url
  reg = call1 (ex "navigator" !. "serviceWorker" !. "register") urlStr
  in ifonly cond $ bare reg

then_ promise handler = call1 (promise !. "then") handler
catch promise handler = call1 (promise !. "catch") handler

-- ** Install

-- | Cache all argument URLs
addAll :: [URL] -> Expr Cache -> Promise ()
addAll urls cache = call1 (cache !. "addAll") (lit (map lit urls))

-- *** Install handlers

addAll' :: [URL] -> M r1 (Expr (ServiceWorkerEvent -> ()))
addAll' urls = newf $ \event -> do
  log "install handler"
  f <- async $ do
    log2 "install handler: add all: " $ lit urls
    cache <- const $ Await $ open "cache" caches
    bare $ Await $ addAll urls cache
  bare $ waitUntil (call0 f) event

-- *** Fetch

respondWith :: Promise Response -> Expr ServiceWorkerEvent -> Expr ()
respondWith promise fetchEvent = call1 (fetchEvent !. "respondWith") promise

-- ** Generation

declareFields [d|
  data Gen = Gen
    { genInstallCache :: [URL]
    , genCacheNetworkFallback :: [URL]
    , genNetworkCacheFallback :: [URL]
    , genCacheOnly :: [URL]
    , genNetworkOnly :: [URL]
    , genCacheNetworkRace :: [URL]
    }
   |]

instance Default Gen where
  def = Gen mempty mempty mempty mempty mempty mempty

generate :: Gen -> M r ()
generate gen = do
  installHandler <- addAll' $ gen^.installCache
  fetchHandler <- newf $ \(event :: Expr ServiceWorkerEvent) -> do
    genCode event defaultFetch
       $ map (cacheNetwork event) (gen^.cacheNetworkFallback)
      <> map (cacheOnly event) (gen^.installCache)

  bare $ DOM.addEventListener self DOM.Install installHandler
  bare $ DOM.addEventListener self DOM.Fetch fetchHandler
  where
    genCode :: Expr ServiceWorkerEvent -> (Expr ServiceWorkerEvent -> M r ()) -> [(Expr Bool, M r ())] -> M r ()
    genCode event defaultFetch li = foldl f (defaultFetch event) li
      where f rest (cond, code) = ifelse cond code rest

    mkCond :: Expr ServiceWorkerEvent -> URL -> Expr Bool
    mkCond event url' = url (request event) .=== lit (renderURL url')

    cacheOnly :: Expr ServiceWorkerEvent -> URL -> (Expr Bool, M r ())
    cacheOnly event url' = let
      code = do
        req <- const $ request event
        p <- promise $ do
          cache <- const $ Await $ open "cache" caches
          resp <- const $ Await $ match req cache
          log2 "fetch: cache only:" $ url req
          retrn resp
        bare $ respondWith p event
      in (mkCond event url', code)

    cacheNetwork :: Expr ServiceWorkerEvent -> URL -> (Expr Bool, M r ())
    cacheNetwork event url' = let
      code = do
        req <- const $ request event
        p <- promise $ do
          cache :: Expr Cache <- const $ Await $ open "cache" caches
          resp :: Expr Response <- const $ Await $ match req cache
          ifelse (Cast resp) (
            do log2 "fetch: cache hit:" $ url req
               retrn resp
            ) (
            do log2 "fetch: cache miss:" $ url req
               resp <- fetchAndCache req cache
               log2 "fetch: return network response:" $ url req
               retrn resp
            )
        bare $ respondWith p event
      in (mkCond event url', code)

    defaultFetch :: Expr ServiceWorkerEvent -> M r ()
    defaultFetch event = log2 "fetch: url" $ url (request event)

fetchAndCache req cache = do
  resp :: Expr Response <- const $ Await $ fetch (Cast req) []
  putCache <- async $ do -- created to see that it happens async
    bare $ Await $ put req (clone resp) cache
  bare $ call0 putCache
  return resp

pwaDiagnostics = do
  listCaches <- api $ return $ \_ -> do
    cssRule body $ do
      whiteSpace "pre"
    js $ do
      mklink <- newf $ \url -> do
        retrn $ "<a href='" + url + "'>" + url + "</a>"
      withCache <- async $ \cacheName -> do
        cache <- const $ Await $ open cacheName caches
        requests <- const $ Await $ keys cache
        g <- newf $ \req -> retrn $ url req
        urls <- const $ call1 (requests !. "map") g
        let links = call1 (urls !. "map") mklink
        retrn $ cacheName + ":<br/>- " + (JS.join "<br/>- " links)
      main <- async $ do
        keys <- const $ Await $ keys caches
        str <- const $ Await $ call1 (ex "Promise" !. "all") $ call1 (keys !. "map") withCache

        bare $ DOM.documentWrite str
      bare $ DOM.addEventListener (Cast DOM.window) DOM.Load (Cast main)

    dest <- cssId $ pure ()
    return $ htmlDoc (pure ()) $ do
      div ! dest $ ""

  _ <- pin "pwa-diag" $ return $ \_ -> do
    return $ htmlDoc (pure ()) $ a ! href listCaches $ "list caches"

  return ()
