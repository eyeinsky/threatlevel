module Static where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as TS
import qualified Data.Text.Strict.Lens as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Network.Mime as Mime
import qualified Network.HTTP.Types as Wai
import System.IO as IO
import System.Process as IO
import Control.Exception
import Language.Haskell.TH
import Data.FileEmbed

import qualified HTTP.Header as Hdr
import qualified HTTP.Response as HR
import qualified Web.Response as WR
import qualified Web.Endpoint as WE

import X.Prelude as P
import X


-- ** File

diskFile :: MonadIO m => FilePath -> m Response
diskFile path = do
  bytes <- liftIO $ BL.readFile path
  return $ rawBl (toEnum 200) [pathContentType path] bytes

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
statics' (pairs :: [(FilePath, BS.ByteString)]) = forM pairs $ \(path, bs) -> let
  mime = path^.TS.packed.to Mime.defaultMimeLookup.from strict & TL.decodeUtf8
  headers = [Hdr.contentType mime]
  response = WR.rawBl (toEnum 200) headers (bs^.from strict)
  path' = TS.pack path
  in (path,) <$> (WE.pin path' $ WE.staticResponse response)

-- | Generate endpoints for source-embedded files and return the html
-- to include them.
includes (pairs :: [(FilePath, BS.ByteString)]) = statics' pairs <&> map f ^ sequence_
  where
    f :: (FilePath, URL) -> Html
    f (path, url) = case P.split "." path^.reversed.ix 0 of
      "css" -> includeCss url
      "js" -> includeJs url
      _ -> pure ()

-- | Serve the subtree at fp from disk. The url is generated, the rest
-- needs to match file's path in the. TODO: resolve ".." in path and
-- error out if path goes outside of the served subtree. And check the
-- standard of if .. is even allowed in url paths.
staticDiskSubtree' mod onError (path :: FilePath) = do
  return $ \(_ :: Request) -> do
    e <- asks (view WE.dynPath) <&> sanitizePath
    case e of
      Left e -> onError e
      Right subPath -> mod <$> diskFile (path <> "/" <> subPath)

sanitizePath :: [TS.Text] -> Either TS.Text FilePath
sanitizePath parts = if any (== "..") parts
  then Left "Not allowed to go up"
  else Right (TS.unpack $ TS.intercalate "/" parts)

-- | Serve entire path from under created url
staticDiskSubtree notFound path = staticDiskSubtree' P.id (\_ -> return notFound) path

-- | Serve files from filesystem path using a content adressable hash
assets
  :: (MonadReader URL m, MonadIO m, MonadWriter [(Segment, T s)] m, Confy s)
  => WR.Response -> String -> m URL
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
    cmd = "tar cf - '" <> path <> "' | sha1sum | cut -d ' ' -f 1"
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
