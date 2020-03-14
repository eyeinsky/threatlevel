module Main where

import qualified Data.Text as TS
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Rapid

import X.Prelude
import JS
import URL.TH (url)
import X
import qualified SVG

declareFields [d|
  data RunConf = RunConf
    { runConfDynPath :: [Segment]
    , runConfBrowser :: Browser
    }
  |]

instance Default RunConf where
  def = RunConf mempty Unknown

main :: IO ()
main = do
  let
    tls = Just $ Warp.tlsSettings
      "tmp/tests.fw.yay.pem"
      "tmp/tests.fw.yay-key.pem"
  siteMain def def [url|https://tests.fw.yay:8087|] 8087 tls (site)

site :: T RunConf
site = T $ mdo
  liftIO $ putStrLn "Site init"
  return $ \_ -> do

    testSection <- styled section $ do
      padding $ rem 1
      borderRadius $ rem 0.2
      border $ px 1 <> "solid" <> "gray"
    testTitle <- styled h3 $ pure ()

    -- * Test 1

    jsCircle <- newId
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

    test2target <- newId
    test2buttonHtml <- newId
    test2buttonJs <- newId
    click <- js $ newf $ log "click"
    let test2button = button ! on Click click $ "click me"
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
    test3container <- newId
    test3element <- newId
    test3target <- newId
    jsAttrValue <- js $ new value
    let
      test3html = div
        ! DynamicA key jsAttrValue
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
      p "There should be no errors on console."
      test1
      test2
      test3


hot, stop :: IO ()
hot = Rapid.rapid 0 $ \r -> do
  Rapid.restart r ("webserver" :: String) main
stop = Rapid.rapid 0 $ \r -> do
  Rapid.stop r ("webserver" :: String) main
