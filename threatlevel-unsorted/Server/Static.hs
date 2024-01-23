module Server.Static where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as TS
import qualified Data.Text.Strict.Lens as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Lens as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.Mime as Mime
import qualified Network.HTTP.Types as Wai
import System.IO as IO
import Control.Exception
import System.Process as IO
import Language.Haskell.TH
import Data.FileEmbed

import qualified HTTP.Response as HR
import Server.Response as R
import Server.API

import Web
import Network.Wai
import Common.Prelude as P

-- ** File

diskFile :: MonadIO m => FilePath -> m (Either IOException R.Response)
diskFile path = do
  either <- liftIO $ try $ BL.readFile path
  return $ rawBl (toEnum 200) [pathContentType path] <$> either

-- | Embeds a file from path into binary, resulting file's type will
-- be Response.
embeddedFile :: TL.Text -> ExpQ
embeddedFile path = let
    filePath = path^.from TL.packed :: FilePath
    ct = path^.from lazy.to Mime.defaultMimeLookup.TS.utf8.from TS.packed & stringE
  in [| let header = (Wai.hContentType, $ct)
        in rawBS 200 [header] $(embedFile filePath)
           :: R.Response|]

-- | Serve source-embedded files by their paths. Note that for dev
-- purposes the re-embedding of files might take too much time.
statics' (pairs :: [(FilePath, BS.ByteString)]) = forM pairs $ \(path, bs) -> let
  mime = path^.TS.packed.to Mime.defaultMimeLookup
  headers = [(Wai.hContentType, mime)]
  response = R.rawBl (toEnum 200) headers (bs^.from strict)
  path' = TS.pack path
  in (path,) <$> (pin path' $ staticResponse response)

-- | Generate endpoints for source-embedded files and return the html
-- to include them.
includes (pairs :: [(FilePath, BS.ByteString)]) = statics' pairs <&> map f ^ sequence_
  where
    f :: (FilePath, URL) -> Html
    f (path, url) = case P.split "." path^.reversed.ix 0 of
      "css" -> stylesheet url
      "js" -> includeJs url
      _ -> pure ()

-- | Serve the subtree at fp from disk. The url is generated, the rest
-- needs to match file's path in the. TODO: resolve ".." in path and
-- error out if path goes outside of the served subtree. And check the
-- standard of if .. is even allowed in url paths.
staticDiskSubtree' mod onError (path :: FilePath) = do
  return $ \(_ :: Request) -> do
    e <- asks (view dynPath) <&> sanitizePath
    case e of
      Left e -> onError e
      Right subPath -> do
        either <- diskFile (path <> "/" <> subPath)
        case either of
          Left ioException -> onError $ TS.pack $ show ioException
          Right response -> return $ mod response

sanitizePath :: [TS.Text] -> Either TS.Text FilePath
sanitizePath parts = if any (== "..") parts
  then Left "Not allowed to go up"
  else Right (TS.unpack $ TS.intercalate "/" parts)

-- | Serve entire path from under created url
staticDiskSubtree notFound path = staticDiskSubtree' P.id (\_ -> return notFound) path

-- | Serve files from filesystem path using a content adressable hash
-- assets :: (API m s, MonadIO m, Confy s) => R.Response -> FilePath -> m URL
-- assets notFound path = do
--   hashPin path $ staticDiskSubtree' headerMod (\_ -> return notFound) path
--   where
--     headerMod = R.headers <>~ [HR.cacheForever]
--     hashPin path what = do
--       hash <- liftIO (folderHash path) <&> TS.pack
--       liftIO $ print (path, hash)
--       pin hash what

folderHash :: String -> IO [Char]
folderHash path = do
  (_, o, e, _) <- IO.runInteractiveCommand cmd
  IO.hGetContents e >>= hPutStrLn stderr
  IO.hGetContents o <&> P.take 40
  where
    cmd = "tar -h -c -f - '" <> path <> "' | sha1sum | cut -d ' ' -f 1"
    -- todo: better path escaping

folderHashTH :: FilePath -> ExpQ
folderHashTH path = runIO (folderHash path) >>= stringE

pathContentType :: FilePath -> Wai.Header
pathContentType path = (Wai.hContentType, mime)
  where
    mime = Mime.defaultMimeLookup $ path^.TS.packed :: BS.ByteString
--    mime' = mime^.TS.utf8.from strict :: TL.Text

-- * New API

-- | Serve static file from disk which can change or is very
-- large. Otherwise consider using @embedFile@ to include it in the
-- binary.
serveFile :: API m r => FilePath -> m URL
serveFile filePath = do
  let response = File Wai.status200 [pathContentType filePath] filePath Nothing
  api $ return $ \_ -> return response

-- serveFolder :: (API m r) => Segment -> FilePath -> m URL
-- serveFolder name folderPath = pin name $ return $ \req -> todo
