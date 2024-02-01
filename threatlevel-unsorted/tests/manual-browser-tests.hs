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
import System.IO qualified as IO
import System.Environment qualified as IO
import Control.Concurrent qualified as IO
import Control.Monad.State (MonadState)

import Hedgehog qualified
import Hedgehog.Gen qualified
import Hedgehog.Range qualified
import Test.QuickCheck qualified as Q
import Test.QuickCheck (Arbitrary(arbitrary))


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
import Server.API qualified as API

-- * Helpers

encodeText :: A.ToJSON a => a -> TL.Text
encodeText = TL.decodeUtf8 . A.encode

-- * Scaffold data types

data SingleEnum = SingleEnum deriving (Eq, Ord, Show, Generic)
data MultiEnum = MultiEnumA | MultiEnumB | MultiEnumC deriving (Eq, Ord, Show, Generic, Enum, Bounded)
data SingleDcOneField = SingleDcOneField { singleDcOneField :: Int } deriving (Eq, Ord, Show, Generic)
data SingleDcManyFields = SingleDcManyFields
  { singleDcManyFieldsInt :: Int
  , singleDcManyFieldsString :: String
  } deriving (Eq, Show, Ord, Generic)
data MultiRecordOneField
  = MultiRecordOneField1 { multiRecordOneField1 :: Int }
  | MultiRecordOneField2 { multiRecordOneField2 :: Int }
  | MultiRecordOneField3 { multiRecordOneField3 :: Int }
  | MultiRecordOneField4 { multiRecordOneField4 :: Int }
  deriving (Eq, Ord, Show, Generic)
data MultiRecordManyFields
  = MultiRecordManyFields1 { multiRecordManyFields1 :: Int }
  | MultiRecordManyFields2 { multiRecordManyFields21 :: Int, multiRecordManyFields22 :: Int }
  | MultiRecordManyFields3 { multiRecordManyFields31 :: Int, multiRecordManyFields32 :: Int, multiRecordManyFields33 :: Int }
  | MultiRecordManyFields4 { multiRecordManyFields41 :: Int, multiRecordManyFields42 :: Int, multiRecordManyFields43 :: Int, multiRecordManyFields44 :: Int }
  deriving (Eq, Ord, Show, Generic)
data MultiRecordMixed
  = MultiRecordMixed1_RecordSingleField { multiRecordMixed_RecordSingleField :: Int }
  | MultiRecordMixed1_Enum1
  | MultiRecordMixed1_RecordMultiField { multiRecordMixed_RecordMultiField1 :: Int, multiRecordMixed_RecordMultiField2 :: Int }
  | MultiRecordMixed1_Enum2
  deriving (Eq, Ord, Show, Generic)
data AnyData
  = AnyData_SingleEnum SingleEnum
  | AnyData_MultiEnum MultiEnum
  | AnyData_SingleDcOneField SingleDcOneField
  | AnyData_SingleDcManyFields SingleDcManyFields
  | AnyData_MultiRecordOneField MultiRecordOneField
  | AnyData_MultiRecordManyFields MultiRecordManyFields
  | AnyData_MultiRecordMixed MultiRecordMixed
  deriving (Eq, Ord, Show, Generic)

-- ** Arbitrary instances

instance Arbitrary SingleEnum where arbitrary = return SingleEnum
instance Arbitrary MultiEnum where arbitrary = Q.arbitraryBoundedEnum
instance Arbitrary SingleDcOneField where arbitrary = SingleDcOneField <$> arbitrary
instance Arbitrary SingleDcManyFields where arbitrary = SingleDcManyFields <$> arbitrary <*> arbitrary
instance Arbitrary MultiRecordOneField where
  arbitrary = Q.oneof
    [ MultiRecordOneField1 <$> arbitrary
    , MultiRecordOneField2 <$> arbitrary
    , MultiRecordOneField3 <$> arbitrary
    , MultiRecordOneField4 <$> arbitrary
    ]
instance Arbitrary MultiRecordManyFields where
  arbitrary = Q.oneof
    [ MultiRecordManyFields1 <$> arbitrary
    , MultiRecordManyFields2 <$> arbitrary <*> arbitrary
    , MultiRecordManyFields3 <$> arbitrary <*> arbitrary <*> arbitrary
    , MultiRecordManyFields4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]
instance Arbitrary MultiRecordMixed where
  arbitrary = Q.oneof
    [ MultiRecordMixed1_RecordSingleField <$> arbitrary
    , pure MultiRecordMixed1_Enum1
    , MultiRecordMixed1_RecordMultiField <$> arbitrary <*> arbitrary
    , pure MultiRecordMixed1_Enum2
    ]
instance Arbitrary AnyData where
  arbitrary = Q.oneof
    [ AnyData_SingleEnum <$> arbitrary
    , AnyData_MultiEnum <$> arbitrary
    , AnyData_SingleDcOneField <$> arbitrary
    , AnyData_SingleDcManyFields <$> arbitrary
    , AnyData_MultiRecordOneField <$> arbitrary
    , AnyData_MultiRecordManyFields <$> arbitrary
    , AnyData_MultiRecordMixed <$> arbitrary
    ]

-- ** JSON

instance A.ToJSON SingleEnum
instance A.ToJSON MultiEnum
instance A.ToJSON SingleDcOneField
instance A.ToJSON SingleDcManyFields
instance A.ToJSON MultiRecordOneField
instance A.ToJSON MultiRecordManyFields
instance A.ToJSON MultiRecordMixed
instance A.ToJSON AnyData

instance A.FromJSON SingleEnum
instance A.FromJSON MultiEnum
instance A.FromJSON SingleDcOneField
instance A.FromJSON SingleDcManyFields
instance A.FromJSON MultiRecordOneField
instance A.FromJSON MultiRecordManyFields
instance A.FromJSON MultiRecordMixed
instance A.FromJSON AnyData

makeFields ''SingleDcManyFields
deriveToExpr ''SingleDcManyFields
-- A.deriveJsonNoTypeNamePrefix ''SingleDcManyFields

-- * Upstream

htmlResponse :: Wai.Status -> [Wai.Header] -> Html -> Wai.Response
htmlResponse status headers html = Wai.responseBuilder status headers . addDoctype . BL.fromLazyByteString . TL.encodeUtf8 $ render' html
  where
    addDoctype = ("<!doctype html>" <>)

-- * Test scaffolding

warpSettings :: IO (Maybe Warp.TLSSettings, Warp.Settings)
warpSettings = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  maybeTls <- Wai.tlsSettingsEnv "DEV_WEBSERVER_CERT" "DEV_WEBSERVER_KEY"
  let settings = Warp.setPort 8087 Warp.defaultSettings
  mapM_ print [("port", show 8087), ("TLS", show $ isJust maybeTls)]
  return (maybeTls, settings)

-- | Run @Web Html@ in warp, with TLS from environment (DEV_WEBSERVER_CERT DEV_WEBSERVER_KEY) if available.
runWarpWeb :: Web Html -> IO ()
runWarpWeb web = do
  (maybeTls, settings) <- warpSettings
  Wai.runWarp maybeTls settings $ \_req respond -> do
    respond $ htmlResponse (toEnum 2000) [] $ htmlDoc mempty web

runWarpApi :: API.T [Segment] -> IO ()
runWarpApi t = do
  (maybeTls, settings) <- warpSettings
  let siteBaseUrl = [url|https://localhost.yay:8087|] :: URL
  API.siteMain maybeTls siteBaseUrl settings [] t

instance API.HasDynPath [Segment] [Segment] where

-- * Main

hot, stop_ :: IO ()
(hot, stop_) = mkHot "webserver" main

main :: IO ()
main = runWarpApi jsonRoundtripTest

-- * Roundtrip

jsonRoundtripTest :: API.T [Segment]
jsonRoundtripTest = API.T $ do
  mvar :: IO.MVar AnyData <- liftIO $ IO.newEmptyMVar

  compare <- API.pin "json-compare" $ return $ \_req -> do
    A.eitherDecode <$> liftIO (Wai.getRequestBody _req) >>= \case
      Left err -> do
        liftIO $ putStrLn err
        return $ API.json False
      Right a -> do
        sent <- liftIO $ IO.takeMVar mvar
        return $ API.json (sent == a)

  json <- API.pin "json-new" $ return $ \_req -> do
    a <- liftIO $ Q.generate Q.arbitrary
    liftIO $ IO.putMVar mvar a
    return $ API.json a

  return $ \_req -> do
    return $ API.page $ htmlDoc mempty $ do

      jsonCompare <- async $ do
        resp <- const $ Await $ fetch (lit json) []
        jsonValue <- const $ Await $ resp !/ "json"
        send <- const $ Await $ fetchPost (lit compare) (jsonBody jsonValue)
        return_ $ Await $ send !/ "json"


      jsonRoundtripElem <- css $ pure ()
      reportResult <- fn $ \(name :: Expr String) -> do
        querySelector jsonRoundtripElem document !. "innerHTML" .=$ name
        return_ (Null :: Expr ())

      onEvent Load window $ do
        i <- let_ 0
        successes <- let_ 0
        result <- let_ Null
        querySelector jsonRoundtripElem document !. "innerHTML" .=$ "RUNNING..."
        while (i .< 100) $ do
          result .= Await (call0 jsonCompare)
          ifonly result $ successes .= successes + 1
          i .= i + 1

        res :: Expr String <- const $ " (" + Cast successes + "/" + Cast i + ")"
        ifelse (i .== successes)
          (bare $ reportResult $ "SUCCESS" + res)
          (bare $ reportResult $ "FAIL" + res)

      return $ do
        h1 "Tests"
        ul $ do
          li $ do
            p $ do
              label "JSON roundtrip: "
              span ! jsonRoundtripElem $ "NOT RUNNING"
            p "Generates arbitrary data in back-end, queried and parsed by front-end, then reserialised and sent back, then parsed again and compared."


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
