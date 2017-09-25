module Main where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Control.Monad.Writer
import qualified Network.Wai as Wai

import Rapid

import Prelude2 hiding (div)
import Web
import DOM
import HTML
import qualified SVG
import Warp_Helpers
import URL
import HTTP.Response
import qualified Web.Response as Re
import Web.Endpoint
import JS hiding (browser)
import qualified JS.Render

declareFields [d|
  data Conf = Conf
    { confDynPath :: [TL.Text]
    , confBrowser :: Browser
    }
  |]

update = rapid 0 (\r ->
  restart r "app" main)

main = runServer $ Server Nothing [("name", endpointHandler)] [("name", auth, 8080)]
  where
    rootUrl =  localhost & port .~ 8080
    auth = rootUrl^.authority

    simpleHandler :: Authority -> IO Handler
    simpleHandler auth = do
      -- init & binds
      return $ \req -> return $ toRaw $ toResponse $ Re.page $ do
        p "paragraph"

    endpointHandler :: Authority -> IO Handler
    endpointHandler auth = do
      -- init & binds
      let
        browser = Unknown
        jsConf = JS.Conf Unknown True $ JS.Render.Indent 2
        mwConf = Web.Conf jsConf browser
        userConf = Main.Conf mempty Unknown :: Main.Conf
      return (toHandler' mwConf localhost userConf testRawHtml)


site = T $ do
  lift $ reset
  return $ do
    let
      s = do
        width $ px 400
        height $ px 400
        border $ px 1 <> "solid" <> "red"
    c1 <- css s
    id <- css s
    let
      svg' :: SVG.Svg
      svg' = SVG.svg ! SVG.viewBox "0 0 1 1" $
        SVG.path
          ! SVG.d ("M 0 1 l 0.5 -0.9 l 0.5 0.9")
          ! SVG.stroke "blue"
          ! SVG.strokeWidth "0.01"
          ! SVG.fill "transparent"
          $ pure ()

    -- js $ bare $ appendChild (call0 $ createHtmls $ embed svg') (findBy id)
    return $ Re.page $ HTML.body $ do
      div ! c1 $ do
        let
        embed svg'
      div ! id $ do
        let
        embed svg'

testRawHtml = T $ do
  return $ do
    id <- newId
    buttonId <- newId
    c <- css' $ color "blue"
    c1 <- css' $ color "green"
    c2 <- css' $ color "gray"
    addRaw <- js $ newf $ do
      let
        html = do
          div ! c $ "div element"
          "some text" :: Html
          div ! c1 $ "START"
          raw "some text 2<div>div in raw<span>span in raw</span>div raw 2</div>more text<b>bold in raw</b>"
          div "END"
      bare $ appendChild (call0 $ domExpr html) (findBy id)
    js $ bare $ addEventListener (findBy buttonId) Click addRaw
    return $ Re.page $ HTML.body $ do
      div ! c $ "blue"
      div ! c1 $ "green"
      div ! c2 $ "gray"
      div ! id $ do
        "initial text"
      button ! buttonId $ "button"

toHandler'
  :: (HasDynPath r Web.Endpoint.Path, HasBrowser r Browser)
  => Web.Conf -> UrlPath -> r -> T r -> Wai.Request -> IO Raw
toHandler' mc host conf site req = toHandler mc host conf' site req <&> fromJust ^ toResponse ^ toRaw
  where
    hdrs = Wai.requestHeaders req
    browser' = maybe Unknown parseBrowser $ lookup "User-Agent" hdrs
    conf' = conf & browser .~ browser'
