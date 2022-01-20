{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Rapid
import qualified Data.Aeson as A
import qualified Data.Aeson.DeriveNoPrefix as A

import X.Prelude
import JS
import JS.TH
import JS.Roundtrip
import URL.TH (url)
import X
import qualified SVG

import Module1

-- * Helpers

encodeText :: A.ToJSON a => a -> TL.Text
encodeText = TL.decodeUtf8 . A.encode

-- * Data types

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


declareFields [d|
  data RunConf = RunConf
    { runConfDynPath :: [Segment]
    }
  |]

instance Default RunConf where
  def = RunConf mempty Unknown

main :: IO ()
main = do
  let
    settings = Warp.setPort 8087 Warp.defaultSettings
    tls = Just $ Warp.tlsSettings
      "tmp/all-certs.pem"
      "tmp/all-certs-key.pem"
  siteMain def def [url|https://tests.fw.yay:8087|] settings tls (site)


site :: T RunConf
site = T $ mdo
  liftIO $ putStrLn "Site init"

  roundtrip <- pin "roundtrip" $ return $ \_ -> do
    f <- js $ newf $ \(event :: Expr MouseEvent) -> log event
    c <- css $ pure ()
    dest <- cssId $ pure ()
    code' <- styleds code $ do
      padding $ rem 0.2 <> rem 0.3
      borderRadius $ rem 0.2
      backgroundColor "black"
      color "white"
    addLi <- js $ newf $ \(label' :: Expr String) (what :: Expr String) -> do
      fragment <- createHtmls $ tr $ do
        td $ toHtml label'
        td $ code' $ toHtml what
      bare $ findBy dest !// "append" $ fragment
    js $ onEvent Load window $ do

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
      bare $ call addLi ["singleDC'^.int", stringify $ singleDC'^.int]
      bare $ call addLi ["singleDC'^.string", stringify $ singleDC'^.string]
      bare $ call addLi ["int eq", stringify $ lit (singleDC^.int) .=== singleDC'^.int]
      bare $ call addLi ["string eq", stringify $ lit (singleDC^.string) .=== singleDC'^.string]

      -- * Multi-constructor => prisms




    return $ htmlDoc (includeJs [url|https://cdn.jsdelivr.net/npm/lodash@4.17.19/lodash.min.js|]) $ body $ do
      h1 "Roundtrippin'" ! On Click f ! c
      table "" ! dest

  jsTryCatch <- pin "js-try-catch" $ return $ \_ -> do

    js $ do
      log "tryCatch"
      tryCatch
        (log "try, throws 123" >> throw "123")
        (\e -> log "catch" >> log e)
      tryCatchFinally
        (log "try, throws 234" >> throw "234")
        (\e -> log "catch" >> log e)
        (log "finally")

    return $ htmlDoc "" ""

  asyncIterator <- pin "async-iterator" $ return $ \_ -> do

    testAsyncIter <- cssId $ pure ()
    stop <- cssId $ pure ()
    _ <- js $ onEvent Load window $ do
      iter <- iterEvent Click (findBy testAsyncIter)
      _ <- onEvent Click (findBy stop) $ do
        bare $ iter !/ "return"
        log "stop"
      forAwait iter $ \ev -> do
        log2 "event" ev
        log2 "iter" iter
      log "after for await"
    return $ htmlDoc "" $ do
      button ! testAsyncIter $ "test button"
      button ! stop $ "stop"

  suffixRepeatedNames <- pin "suffix-repeated-names" $ return $ \_ -> do
    js $ replicateM_ 10 $ new' "test" Undefined
    return $ htmlDoc ""
      "Check page source: all js variables with name \"test\" should be made unique unique with a prefix."

  _ <- pin "JS.TH" $ return $ \_ -> do

    dest <- cssId $ pure ()

    let singleDC = SingleDC 123 "abc"
        singleDC' = lit singleDC :: Expr SingleDC

    f <- js $ newf $ \(str :: Expr String) -> do
      log str
      h <- createHtmls $ li $ toHtml str
      log h
      bare $ findBy dest !// "append" $ h

    _ <- js $ onEvent Load window $ do

      bare $ call1 f $ stringify singleDC'
      log "done"

    return $ htmlDoc "" $ do
      div "Result"
      ul ! dest $ ""

  return $ \_ -> do

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
    js $ do
      addCircle <- newf $ do
        df <- createHtmls circle
        bare $ findBy jsCircle !// "append" $ df
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
    click <- js $ newf $ log "click"
    let test2button = button ! On Click click $ "click me"
    js $ do
      bare . addEventListener (Cast window) Load =<<$ newf $ do
        fragment <- createHtmls $ test2button ! test2buttonJs
        bare $ findBy test2target !// "append" $ fragment
        bare $ findBy test2buttonHtml !/ "click"
        bare $ findBy test2buttonJs !/ "click"
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
    jsAttrValue <- js $ const value
    let
      test3html = div
        ! Custom key (Dynamic jsAttrValue)
        ! test3element
        $ do
          "My attribute \"" <> toHtml key <> "\" has value"
          text " \""
          X.span ! test3target $ ""
          text "\""

    js $
      bare . addEventListener (Cast window) Load =<<$ newf $ do
        fragment <- createHtmls test3html
        bare $ findBy test3container !// "append" $ fragment
        bare $ findBy test3target !// "append" $
          findBy test3element !. key
    let test3 = testSection $ do
          testTitle "Dynamic attribute"
          div ! test3container $ ""


    return $ htmlDoc "" $ do
      h1 "Tests"
      ul $ do
        li $ a ! href jsTryCatch $ "jsTryCatch"
        li $ a ! href suffixRepeatedNames $ "suffixRepeatedNames"
        li $ a ! href asyncIterator $ "asyncIterator"
        li $ a ! href roundtrip $ "roundtrip"
      p "There should be no errors on console."
      test1
      test2
      test3


hot, stop :: IO ()
hot = Rapid.rapid 0 $ \r -> do
  Rapid.restart r ("webserver" :: String) main
stop = Rapid.rapid 0 $ \r -> do
  Rapid.stop r ("webserver" :: String) main
