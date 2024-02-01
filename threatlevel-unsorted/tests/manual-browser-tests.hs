{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Data.Text qualified as TS
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Network.HTTP.Types qualified as Wai
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Data.Aeson qualified as A
import Data.Aeson.DeriveNoPrefix qualified as A
import qualified Blaze.ByteString.Builder as BL

import Common.Prelude
import JS
import JS.TH
import JS.Roundtrip
import qualified SVG

import DOM.JS
import HTML qualified
import Web
import Server.Wai qualified as Wai
import Server.Hot (mkHot)

-- * Helpers

encodeText :: A.ToJSON a => a -> TL.Text
encodeText = TL.decodeUtf8 . A.encode

-- * Scaffold data types

data SingleDC = SingleDC
  { singleDCInt :: Int
  , singleDCString :: String
  } deriving (Eq, Show, Data, Generic)

makeFields ''SingleDC
deriveToExpr ''SingleDC
A.deriveJsonNoTypeNamePrefix ''SingleDC

data MultiDC
  = First Int  String
  | Second Double String

makeClassyPrisms ''MultiDC

-- * Upstream

htmlResponse :: Wai.Status -> [Wai.Header] -> Html -> Wai.Response
htmlResponse status headers html = Wai.responseBuilder status headers . addDoctype . BL.fromLazyByteString . TL.encodeUtf8 $ render' html
  where
    addDoctype = ("<!doctype html>" <>)

-- | Run @Web Html@ in warp, with TLS from environment (DEV_WEBSERVER_CERT DEV_WEBSERVER_KEY) if available.
runWarpWeb :: Web Html -> IO ()
runWarpWeb web = do
  maybeTls <- Wai.tlsSettingsEnv "DEV_WEBSERVER_CERT" "DEV_WEBSERVER_KEY"
  maybe (putStrLn "No TLS") (\_ -> putStrLn "TLS") maybeTls
  let settings = Warp.setPort 8087 Warp.defaultSettings
  Wai.runWarp maybeTls settings $ \_req respond -> do
    respond $ htmlResponse (toEnum 2000) [] $ htmlDoc mempty web

-- * Main

hot, stop_ :: IO ()
(hot, stop_) = mkHot "webserver" main

main :: IO ()
main = runWarpWeb tryCatchSyntax

roundtrip :: Web Html
roundtrip = do
  f <- newf $ \(event :: Expr MouseEvent) -> log event
  c <- css $ pure ()
  dest <- cssId $ pure ()
  code' <- styleds code $ do
    padding $ rem 0.2 <> rem 0.3
    borderRadius $ rem 0.2
    backgroundColor "black"
    color "white"
  addLi <- newf $ \(label' :: Expr String) (what :: Expr String) -> do
    fragment <- createHtmls $ tr $ do
      td $ dyn label'
      td $ code' $ dyn what
    bare $ querySelector dest document !// "append" $ fragment

  onEvent Load window $ do
    let
      lodash = ex "_"

      singleDCInt = 123
      singleDCString = "456"
      singleDC = SingleDC {singleDCInt, singleDCString} :: SingleDC
      lit_singleDC = lit singleDC
      encode_singleDC = fromJSON $ lit $ encodeText singleDC

      singleDC' = obj SingleDC (lit singleDCInt) (lit singleDCString) -- <= :: Expr SingleDC

    -- * Single constructor record => lens

    bare $ call addLi ["lit $ show singleDC", lit $ show singleDC]
    bare $ call addLi ["lit singleDC", stringify lit_singleDC]
    bare $ call addLi ["lit $ A.toJSON singleDC", stringify encode_singleDC]
    bare $ call addLi ["_.isEqual(lit, A.toJSON)", call (lodash !. "isEqual") [lit_singleDC, encode_singleDC]]
    bare $ call addLi ["lit $ singleDC^.int", stringify $ lit $ singleDC^.int]
    bare $ call addLi ["lit $ singleDC^.string", stringify $ lit $ singleDC^.string]
    bare $ call addLi ["singleDC'", stringify singleDC']
    -- bare $ call addLi ["singleDC'^.int", stringify $ singleDC'^.int]
    -- bare $ call addLi ["singleDC'^.string", stringify $ singleDC'^.string]
    -- bare $ call addLi ["int eq", stringify $ lit (singleDC^.int) .=== singleDC'^.int]
    -- todo bare $ call addLi ["string eq", stringify $ lit (singleDC^.string) .=== singleDC'^.string]

    -- * Multi-constructor => prisms

  -- before body
--  let _ = htmlDoc (includeJs [url|https://cdn.jsdelivr.net/npm/lodash@4.17.19/lodash.min.js|])

  return $ body $ do
    h1 "Roundtrippin'" ! On Click f ! c
    table "" ! dest

tryCatchSyntax :: Web Html
tryCatchSyntax = do
  log "tryCatch syntax test"
  let common = do
        color "red"
        fontWeight "bold"
  tryCatch_catch <- css common
  tryCatchFinally_catch <- css common
  tryCatchFinally_finally <- css common
  visible_ <- css $ do
    color $ "green" <> important

  onEvent Load window $ do
    tryCatch
      (log "tryCatch, throwing 123" >> throw "123")
      (\e -> do
          log2 "caught" e
          addClass (jsClassName visible_) $ querySelector tryCatch_catch document
      )
    tryCatchFinally
      (log "tryCatchFinally, throwing 234" >> throw "234")
      (\e -> do
          log2 "caught" e
          addClass (jsClassName visible_) $ querySelector tryCatchFinally_catch document
      )
      (do
          log "finally"
          addClass (jsClassName visible_) $ querySelector tryCatchFinally_finally document
      )

  return $ do
    h2 "Try/catch syntax"
    ol $ do
      li ! tryCatch_catch $ "Caught try {..} catch {..}"
      li ! tryCatchFinally_catch $ "Catch in try {..} catch {..} finally {..}"
      li ! tryCatchFinally_finally $ "Finally in try {..} catch {..} finally {..}"

asyncIterator :: Web Html
asyncIterator = do
  testAsyncIter <- cssId $ pure ()
  stop <- cssId $ pure ()
  _ <- onEvent Load window $ do
    iter <- iterEvent Click (querySelector testAsyncIter document)
    _ <- onEvent Click (querySelector stop document) $ do
      bare $ iter !/ "return"
      log "stop"
    forAwait iter $ \ev -> do
      log2 "event" ev
      log2 "iter" iter
    log "after for await"
  return $ do
    button ! testAsyncIter $ "test button"
    button ! stop $ "stop"

suffixRepeatedNames = do
  todo -- replicateM_ 10 $ new' "test" Undefined
  return $ "Check page source: all js variables with name \"test\" should be made unique unique with a prefix."

js_th = do
  dest <- cssId $ pure ()

  let singleDC = SingleDC 123 "abc"
      singleDC' = lit singleDC :: Expr SingleDC

  f <- newf $ \(str :: Expr String) -> do
    log str
    h <- todo -- createHtmls $ li $ toHtml str
    log h
    bare $ querySelector dest document !// "append" $ h

  _ <- onEvent Load window $ do
    bare $ call1 f $ stringify singleDC'
    log "done"

  return $ do
    div "Result"
    ul ! dest $ ""

home = do
  testSection <- styled section $ do
    padding $ rem 1
    borderRadius $ rem 0.2
    border $ px 1 <> "solid" <> "gray"
  testTitle <- styled h3 $ pure ()

  -- * Test 1

  jsCircle <- cssId $ pure ()
  let circle = embed $ SVG.svg
        ! SVG.width "100"
        ! SVG.height "100"
        $ SVG.circle
          ! SVG.cx "50"
          ! SVG.cy "50"
          ! SVG.r "40"
          ! SVG.stroke "green"
          ! SVG.strokeWidth "4"
          ! SVG.fill "yellow"
          $ pure ()

  addCircle <- newf $ do
    df <- createHtmls circle
    bare $ querySelector jsCircle document !// "append" $ df
  bare $ addEventListener (Cast window) Load addCircle
  let
    test1 = testSection $ do
      testTitle "No xmlns attribute for svg when added via javascript"
      p "Inline svg:"
      circle
      p "Svg added via javascript:"
      div ! jsCircle $ ""


  -- * Test 2

  test2target <- cssId $ pure ()
  test2buttonHtml <- cssId $ pure ()
  test2buttonJs <- cssId $ pure ()
  click <- newf $ log "click"
  let test2button = button ! On Click click $ "click me"

  bare . addEventListener (Cast window) Load =<<$ newf $ do
    fragment <- createHtmls $ test2button ! test2buttonJs
    bare $ querySelector test2target document !// "append" $ fragment
    bare $ querySelector test2buttonHtml document !/ "click"
    bare $ querySelector test2buttonJs document !/ "click"
  let test2 = testSection $ do
        testTitle "Add event handlers to html created via javascript"
        test2button ! test2buttonHtml
        div ! test2target $ ""

  -- * Test 3: Dynamic attributes

  let
    key = "dynamicKey" :: TS.Text
    value = "dynamically assigned value"
  test3container <- cssId $ pure ()
  test3element <- cssId $ pure ()
  test3target <- cssId $ pure ()
  jsAttrValue <- const value
  let
    test3html = div
      ! Custom key (Dynamic jsAttrValue)
      ! test3element
      $ do
        "My attribute \"" <> toHtml key <> "\" has value"
        text " \""
        span ! test3target $ ""
        text "\""


  bare . addEventListener (Cast window) Load =<<$ newf $ do
    fragment <- createHtmls test3html
    bare $ querySelector test3container document !// "append" $ fragment
    bare $ querySelector test3target document !// "append" $
      querySelector test3element document !. key
  let test3 = testSection $ do
        testTitle "Dynamic attribute"
        div ! test3container $ ""


  return $ do
    h1 "Tests"
    -- ul $ do
    --   li $ a ! href jsTryCatch $ "jsTryCatch"
    --   li $ a ! href suffixRepeatedNames $ "suffixRepeatedNames"
    --   li $ a ! href asyncIterator $ "asyncIterator"
    --   li $ a ! href roundtrip $ "roundtrip"
    p "There should be no errors on console."
    test1
    test2
    test3
