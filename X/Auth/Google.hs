module X.Auth.Google where

import Prelude2 as P hiding ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL

import qualified Network.Wreq as Wreq
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Wai
import Warp_Helpers (getRequestBody)

import Control.Monad.IO.Class
import Control.Monad.Trans

import JS -- todo: add to X?
import qualified DOM
import qualified HTML
import Web.Endpoint -- todo: add to X?
import X

-- * Google auth

platform :: Html
platform = script
  ! HTML.src "https://apis.google.com/js/platform.js"
  ! Custom "async" "true"
  ! Custom "defer" "true"
  $ ""

clientId :: TL.Text -> Html
clientId id = meta
  ! Custom "name" "google-signin-client_id"
  ! Custom "content" (id <> suffix)
  $ pure ()
  where suffix = ".apps.googleusercontent.com"

googleAuthHead :: TL.Text -> Html
googleAuthHead id = do
  platform
  clientId id

-- | Globals
gapi = ex "gapi"
auth2 = gapi !. "auth2"

tokeninfoUrl :: IsString p => p
tokeninfoUrl = "https://www.googleapis.com/oauth2/v3/tokeninfo"

declareFields [d|
  data Tokeninfo = Tokeninfo
    { tokeninfoEmail :: Text
    , tokeninfoPicture :: Text
    , tokeninfoName :: Text
    } deriving Show
  |]

instance Aeson.FromJSON Tokeninfo where
   parseJSON (Aeson.Object v) = Tokeninfo
      <$> v Aeson..: "email"
      <*> v Aeson..: "picture"
      <*> v Aeson..: "name"

instance Aeson.ToJSON Tokeninfo where
  toJSON t = Aeson.object ["email" Aeson..= email']
    where
      email' = Aeson._String # TL.toStrict (t^.email) :: Aeson.Value

googleSignout = do
  auth2 <- new $ call0 (ex "gapi.auth2.getAuthInstance");
  await $ call0 (auth2 !. "signOut")

verifyToken id callback = do
  url <- api $ return $ \req -> do
    bsBody <- liftIO $ getRequestBody req
    let kvs = Wai.parseQueryText $ BL.toStrict bsBody
        token = case kvs of
          (_, Just token') : _ -> token'
          _ -> error "!! no token"
    r <- liftIO $ Wreq.post tokeninfoUrl ["id_token" Wreq.:= token ]
    let mbtokeninfo = r ^? Wreq.responseBody . Aeson._JSON :: Maybe Tokeninfo
    callback $ mbtokeninfo

  doSignIn <- lift $ js $ do

    doGoogleSignIn <- new Undefined
    onLoad <- newf $ do
      -- | Create promise, which, when run, will set the doLogin
      -- variable to a callable function, which will do the login with
      -- the right credentials.
      prepareLogin <- async $ {- \undefined -> -} do
        let params = ulit [("client_id" :: Text, ulit id )]
            prompt = ulit $ [("prompt" :: Text, ulit "select_account")]
        consoleLog ["auth2 load" ]
        googleAuth <- await $ call1 (auth2 !. "init") params
        jsCallback <- newf $ \r -> do
          consoleLog ["jsCallback", r]
        doLogin' <- async $ do
          user <- await $ call1 (googleAuth !. "signIn") prompt
          token <- new $ call0 (user !. "getAuthResponse") !. "id_token"
          -- get url ("?id_token=" .+ token) Undefined
          DOM.xhrJs "POST" (ulit $ renderURL url) ("id_token=" .+ token) []
        doGoogleSignIn .= doLogin'
        consoleLog ["google ready"]

      new $ call (gapi !. "load") ["auth2", prepareLogin]
    bare $ DOM.addEventListener (Cast DOM.window) Load onLoad
    return doGoogleSignIn

  return doSignIn
