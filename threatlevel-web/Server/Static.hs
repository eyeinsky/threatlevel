module Server.Static where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as TS
import qualified Data.Text.Strict.Lens as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.Mime as Mime
import qualified Network.HTTP.Types as Wai
import System.IO as IO
import Control.Exception
import System.Process as IO
import Language.Haskell.TH
import Data.FileEmbed

import qualified HTTP.Header as Hdr
import qualified HTTP.Response as HR
import qualified Server.Response as WR
import qualified Server.API as WE

import X.Prelude as P
import X


-- ** File

diskFile :: MonadIO m => FilePath -> m (Either IOException Response)
diskFile path = do
  either <- liftIO $ try $ BL.readFile path
  return $ rawBl (toEnum 200) [pathContentType path] <$> either

-- | Embeds a file from path into binary, resulting file's type will
-- be Response.
embeddedFile :: TL.Text -> ExpQ
embeddedFile path = let
    filePath = path^.from packed :: FilePath
    ct = path^.from lazy.to Mime.defaultMimeLookup.TS.utf8.from TS.packed & stringE
  in [| let header = Hdr.header Hdr.ContentType $ct
        in rawBS 200 [header] $(embedFile filePath)
           :: Response|]

-- | Serve source-embedded files by their paths. Note that for dev
-- purposes the re-embedding of files might take too much time.
statics' :: (MonadState WE.State m, MonadReader URL m,
             MonadWriter [(Segment, T r)] m) =>
            [(FilePath, BS.ByteString)] -> m [(FilePath, URL)]
statics' (pairs :: [(FilePath, BS.ByteString)]) = forM pairs $ \(path, bs) -> let
  mime = path^.TS.packed.to Mime.defaultMimeLookup.from strict & TL.decodeUtf8
  headers = [Hdr.contentType mime]
  response = WR.rawBl (toEnum 200) headers (bs^.from strict)
  path' = TS.pack path
  in (path,) <$> (WE.pin path' $ WE.staticResponse response)

-- | Generate endpoints for source-embedded files and return the html
-- to include them.
includes :: (MonadState WE.State f, MonadReader URL f,
             MonadWriter [(Segment, T r)] f) =>
            [(FilePath, BS.ByteString)] -> f (WriterT [HTML Both] Identity ())
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
staticDiskSubtree' :: (Monad m2, MonadReader s m1,
                       HasDynPath s [TS.Text], MonadIO m1) =>
                      (Response -> b)
                      -> (TS.Text -> m1 b) -> FilePath -> m2 (Request -> m1 b)
staticDiskSubtree' mod onError (path :: FilePath) = do
  return $ \(_ :: Request) -> do
    e <- asks (view WE.dynPath) <&> sanitizePath
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
staticDiskSubtree :: (MonadReader s m1, HasDynPath s [TS.Text],
                      MonadIO m1, Monad m2) =>
                     Response -> FilePath -> m2 (Request -> m1 Response)
staticDiskSubtree notFound path = staticDiskSubtree' P.id (\_ -> return notFound) path

-- | Serve files from filesystem path using a content adressable hash
assets :: (API m s, MonadIO m, Confy s) => WR.Response -> FilePath -> m URL
assets notFound path = do
  hashPin path $ staticDiskSubtree' headerMod (\_ -> return notFound) path
  where
    headerMod = WR.headers <>~ [HR.cacheForever]
    hashPin path what = do
      hash <- liftIO (folderHash path) <&> TS.pack
      liftIO $ print (path, hash)
      WE.pin hash what

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

pathContentType :: FilePath -> Hdr.Header
pathContentType path = Hdr.header Hdr.ContentType mime'
  where
    mime = Mime.defaultMimeLookup $ path^.TS.packed :: BS.ByteString
    mime' = mime^.TS.utf8.from strict :: TL.Text

-- * New API

-- | Serve static file from disk which can change or is very
-- large. Otherwise consider using @embedFile@ to include it in the
-- binary.
serveFile :: API m r => FilePath -> m URL
serveFile filePath = do
  let response = File Wai.status200 [pathContentType filePath] filePath Nothing
  api $ return $ \_ -> return response

-- serveFolder :: (API m r) => FilePath -> m URL
-- serveFolder folderPath = todo
