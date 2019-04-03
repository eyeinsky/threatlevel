module X.Google.Spreadsheet where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS

import qualified Network.Wreq as Wreq
import Network.Wreq (FormParam ((:=)) )
import qualified Network.HTTP.Client as HC
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Strict.Lens as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Lens as BL

import Control.Exception

import Prelude2


p = Wreq.param

consent clientId scope redirectUri =
  Wreq.getWith opts url
  -- awk '/^location/ {print $2}'
  where
    url = "https://accounts.google.com/o/oauth2/v2/auth"
    opts = Wreq.defaults
      & p "scope"  .~ [scope]
      & p "access_type" .~ ["offline"]
      & p "include_granted_scopes" .~ ["true"]
      & p "redirect_uri" .~ [redirectUri]
      & p "response_type" .~ ["code"]
      & p "client_id" .~ [clientId]

token clientId secret grantType redirect code =
  Wreq.postWith opts url
  where
    url = "https://accounts.google.com/o/oauth2/token"
    opts = Wreq.defaults
      & p "code"  .~ [code]
      & p "redirect_uri"  .~ [redirect]
      & p "grant_type"  .~ [grantType]
      & p "client_secret" .~ [secret]
      & p "client_id"  .~ [clientId]

refresh refreshToken clientId secret = do
  Wreq.post url opts
  where
    url = "https://www.googleapis.com/oauth2/v4/token"
    opts =
      [ "client_id" := clientId
      , "client_secret" := secret
      , "refresh_token" := refreshToken
      , "grant_type" := "refresh_token"
      ]

getSpreadsheet
  :: BS.ByteString -> String -> String -> String
  -> IO (HC.Response BL.ByteString)
getSpreadsheet accessToken docId sheet range =
  Wreq.getWith opts url
  where
    base = "https://sheets.googleapis.com/v4/spreadsheets"
    url = base <> "/" <> docId <> "/values/" <> sheet <> "!" <> range
    opts = Wreq.defaults & Wreq.header "Authorization" .~ (["Bearer " <> accessToken])


get clientId secret docId sheet range refreshToken accessTokenMVar = do
  tokenResponse :: Maybe Aeson.Value <- refresh refreshToken clientId secret
    <&> (^? Wreq.responseBody . Aeson._JSON)
  let maybeToken = tokenResponse ^? _Just . Aeson.key "access_token" . Aeson._String

  case maybeToken of
    Just (token :: TS.Text) -> do
      r <- getSpreadsheet (TS.encodeUtf8 token) docId sheet range
      return $ (r ^? Wreq.responseBody . Aeson._JSON :: Maybe Aeson.Value)
    _ -> return Nothing

  -- r' <- Wreq.getWith req url `catch` \(HC.HttpExceptionRequest req e) -> case e of
  --   HC.StatusCodeException r _ -> let
  --     code = r ^. Wreq.responseStatus
  --     error = r ^. Wreq.responseHeader "WWW-Authenticate"
  --     invalidToken = "invalid_token" `BS.isInfixOf` error
  --     in do
  --     print $ (code, error)


cellValues :: Aeson.Value -> [TL.Text]
cellValues row = x
  where
    x = row ^? Aeson._Array & fromMaybe (pure "-") & toList
      & map (preview Aeson._String ^ fromMaybe "-" ^ view (from strict))
