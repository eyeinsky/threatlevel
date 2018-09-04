module Main where

import Data.Default
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Control.Monad.Writer
import qualified Network.Wai as Wai

import Rapid

import Prelude2 hiding (div, (.=), for, length)
import URL hiding (T)

import qualified Web
import Web as W
import qualified DOM
import DOM (MouseEvent(Click), findBy, addEventListener, domExpr, appendChild, requestAnimationFrame)

import HTML
import qualified SVG
import Warp_Helpers as WH
import URL.ToPayload
import HTTP.Response
import qualified Web.Response as Re
import Web.Endpoint as WE hiding (M)

import JS --  (M, Expr, Arguments)
import qualified JS
import qualified JS.Render



-- * Behavior

-- ** Core

type F1 a1 r = Expr (Arguments (Expr a1 -> M r ()))

type Behavior a = F1 a () -> M () ()
--                    ^ next's input
--             ^ behaviors output

(~>) :: Behavior a -> Behavior a
(b :: Behavior a) ~> (f :: F1 a ()) = do
  g <- new f
  b g
infixr 4 ~>

lift0' :: Expr a -> Behavior a
lift0' a f = bare $ call1 f a

lift1' :: F1 a b -> Behavior a -> Behavior b
lift1' f ba fb = do
  g <- newf $ \(i :: Expr a) -> do
    retrn $ call1 fb (call1 f i)
  ba g

-- ** Implementations

type Time = JS.Number

-- *** DOM

timeB :: Behavior Time
timeB f = do
  r <- new Null
  g <- newf $ \a -> do
    bare $ call1 f a
    bare $ requestAnimationFrame r
  r .= g
  bare $ requestAnimationFrame r

elementNumber :: Expr W.Tag -> Expr JS.Number -> M r ()
elementNumber tag num = do
  tag !. "innerHTML" .= toString num

elementText :: Expr W.Tag -> Expr JS.String -> M r ()
elementText tag str = do
  tag !. "innerHTML" .= str

-- * Event

-- ** Core

type Event a = F1 a () -> M () ()

(==>) :: Event a -> (F1 a b) -> Event b
e ==> f = \g -> do
  h <- newf $ \a -> do
    retrn $ call1 g $ call1 f a
  e h

merge :: Event a -> Event a -> Event a
merge e e' f = do
  e f
  e' f

mergeAll :: [Event a] -> Event a
mergeAll = foldl1 merge

events :: DOM.Event e => e -> W.Id -> Event e
events event id handler = bare $ addEventListener (findBy id) event handler

clicks :: W.Id -> Event DOM.MouseEvent
clicks = events DOM.Click


point = do
  id <- new $ ulit (-1)
  getId <- newf $ do
    id .+= ulit 1
    retrn id
  newf $ \fn -> do
    id <- new $ call0 getId
    observers <- new $ ulit ([] :: [JS.Expr ()])
    add <- newf' "add" $ \listener -> do
      consoleLog [listener !. "id", ulit "added"]
      bare $ push listener observers
    remove <- newf' "remove" $ \listener -> do
      i <- new $ indexOf observers listener
      ifelse (i .>= ulit 0) (do
        removed <- bare $ splice observers i (ulit 1)
        consoleLog [listener !. "id", ulit "removed"]
        retrn true
        ) (do
        consoleLog [listener !. "id", ulit "not found"]
        retrn false
        )
    once <- newf' "once" $ \listener -> do
      send0 <- new $ listener !. "send"
      wrap <- newf $ \ev -> do
        bare $ call1 send0 ev
        retrn $ call1 remove listener
      listener !. "send" .= wrap
      bare $ call1 add listener
    send0 <- newf' "send0" $ \ev -> do
      len <- new $ length observers
      i <- new $ ulit 0
      for (i .< len) $ do
        bare $ call1 ((observers .! i) !.  "send") ev
        i .+= ulit 1
    let ter = ternary (call1 (ex "typeof") fn .=== ulit "function")
    send <- new' "send" =<< (ter <$> (do
      func $ \ev -> do
        bare $ call1 fn ev
        consoleLog [id, ulit "executed"]
        bare $ call1 send0 ev
      ) <*> (func $ \ev -> bare $ call1 send0 ev))

    retrn $ ulit
      [ ("add", Cast add)
      , ("once", Cast once)
      , ("remove", Cast remove)
      , ("send", Cast send)
      , ("observers", Cast observers)
      , ("id", Cast id)
      ]

{-
var p1 = point(function() {
  console.log('jee');
})

var p2 = point(function () {
  var d = new Date()
  document.getElementById('o1').innerHTML = d.toString()
})

var p3 = point(function () {
  var d = new Date()
  document.getElementById('o2').innerHTML = d.toString()
})

p1.add(p2)
p1.once(p3)
-}

-- * Helpers

toHandler'
  :: (WE.HasDynPath r [Segment], HasBrowser r Browser)
  => Web.Conf -> URL.URL -> r -> WE.T r -> Wai.Request -> IO Raw
toHandler' mc host conf site req = WE.toHandler mc host conf' site req <&> fromJust ^ toRaw
  where
    hdrs = Wai.requestHeaders req
    browser' = maybe Unknown parseBrowser $ lookup "User-Agent" hdrs
    conf' = conf & W.browser .~ browser'

-- * Sites

declareFields [d|
  data Conf = Conf
    { confDynPath :: [Segment]
    , confBrowser :: Browser
    }
  |]

localhostHttpServer :: Port -> WE.T Main.Conf -> IO ()
localhostHttpServer port' site = runServer $ Server Nothing
  [("name", handler)]
  [("name", auth, port')]
  where
  auth = URL.localhost & URL.port .~ port' & view authority
  handler _ = return (toHandler' def URL.localhost (def :: Main.Conf) site)


update = rapid 0 (\r ->
  restart r "app" main)

updated name main = rapid 1 (\r ->
  restart r name main)

main = runServer $ Server Nothing [("name", endpointHandler)] [("name", auth, 8080)]
  where
    rootUrl = URL.localhost & port .~ 8080
    auth = rootUrl^.authority

    simpleHandler :: Authority -> IO Handler
    simpleHandler _ = do
      -- init & binds
      return $ \req -> return $ toRaw $ Re.page $ do
        p "paragraph"

    endpointHandler :: Authority -> IO Handler
    endpointHandler _ = return (toHandler' def URL.localhost (def :: Main.Conf) testRawHtml)

instance Default Main.Conf where
  def = Main.Conf mempty Unknown

site = WE.T $ do
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

testRawHtml = WE.T $ do
  return $ do
    id <- newId
    buttonId <- newId
    c <- css $ color "blue"
    c1 <- css $ color "green"
    c2 <- css $ color "gray"
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


(update_lällGälleri, main_lällGälleri) = (updated "lällGälleri" m', m')
  where
    m' = localhostHttpServer 8080 (lällGälleri root')

-- * LällGälleri

data GConf = GConf

root' = "/home/markus/hd/fotod"

bb color = border $ "1px solid " <> color

lällGälleri root = WE.T $ do
  lift $ reset
  body <- lift $ cssId $ do
    display "flex"
    flexFlow "row nowrap"
    bb "red"
  folders <- lift $ cssId $ do
    bb "blue"
    width $ prc 25
  folder <- lift $ css $ do
    bb "blue"
  imgs <- lift $ cssId $ do
    display "flex"
    width $ prc 75
    flexFlow "row wrap"
    bb "green"
    pure ()
  img <- lift $ css $ do
    width $ px 100
    flex "1 1 auto"
    bb "yellow"

  subfolders <- api $ return $ do
    return $ Re.page $ HTML.body ""
  images <- api $ return $ do
    return $ Re.page $ HTML.body ""

  return $ do
    -- request
    -- get
    return $ Re.page $ HTML.body ! body $ do
      div ! folders $ replicateM_ 10 $ do
        div ! folder $ HTML.text "folder"
      div ! imgs $ replicateM_ 10 $ do
        div ! img $ HTML.text "img"
