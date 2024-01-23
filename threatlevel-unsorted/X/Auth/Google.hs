module X.Auth.Google where

import Common.Prelude
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

import qualified Network.Wreq as Wreq
import qualified Network.HTTP.Types as Wai

import qualified HTML
import Web
import Server.API
import Server.Wai qualified as Wai

-- * Google auth

platform :: Html
platform = script
  ! HTML.src "https://apis.google.com/js/platform.js"
  ! Custom "async" "true"
  ! Custom "defer" "true"
  $ ""

clientId :: TS.Text -> Html
clientId id = meta
  ! Custom "name" "google-signin-client_id"
  ! Custom "content" (Static $ id <> suffix)
  $ pure ()
  where suffix = ".apps.googleusercontent.com"

googleAuthHead :: TS.Text -> Html
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
   parseJSON = \case
     Aeson.Object v -> Tokeninfo
       <$> v Aeson..: "email"
       <*> v Aeson..: "picture"
       <*> v Aeson..: "name"
     _ -> todo

instance Aeson.ToJSON Tokeninfo where
  toJSON t = Aeson.object ["email" Aeson..= email']
    where
      email' = Aeson._String # TL.toStrict (t^.email) :: Aeson.Value

googleSignout = do
  auth2 <- const $ call0 (ex "gapi.auth2.getAuthInstance");
  const $ Await $ call0 (auth2 !. "signOut")

verifyToken id callback = do
  url <- api $ return $ \req -> do
    bsBody <- liftIO $ Wai.getRequestBody req
    let kvs = Wai.parseQueryText $ BL.toStrict bsBody
        token = case kvs of
          (_, Just token') : _ -> token'
          _ -> error "!! no token"
    r <- liftIO $ Wreq.post tokeninfoUrl ["id_token" Wreq.:= token ]
    let mbtokeninfo = r ^? Wreq.responseBody . Aeson._JSON :: Maybe Tokeninfo
    callback $ mbtokeninfo

  rc <- ask
  doSignIn <- lift $ do

    doGoogleSignIn <- let_ Undefined
    onLoad <- newf $ do
      -- | Create promise, which, when run, will set the doLogin
      -- variable to a callable function, which will do the login with
      -- the right credentials.
      prepareLogin <- async $ {- \undefined -> -} do
        let params = lit [("client_id" :: Text, lit id )]
            prompt = lit $ [("prompt" :: Text, lit "select_account")]
        log "auth2 load"
        googleAuth <- const $ Await $ call1 (auth2 !. "init") params
        doLogin' <- async $ do
          user <- const $ Await $ call1 (googleAuth !. "signIn") prompt
          token <- const $ call0 (user !. "getAuthResponse") !. "id_token"
          -- get url ("?id_token=" .+ token) Undefined
          -- DOM.xhrJs rc "POST" (lit $ renderURL url) ("id_token=" + token) []
          todo
        doGoogleSignIn .= doLogin'
        log "google ready"

      const $ call (gapi !. "load") ["auth2", prepareLogin]
    bare $ addEventListener (Cast window) Load onLoad
    return doGoogleSignIn

  return doSignIn
