-- * Endpoint

-- exec'
--   :: forall s m b a . (MonadReader s m, MonadWeb m, HasJsConf s JS.Conf)
--   => (Code b -> M b a) -> JS.M b a -> m WR.Response
exec' f jsm = do
  code :: Code b <- js $ mkCode_ jsm
  conf <- js askEnv
  return $ WR.resp200 $ JS conf (f code)

-- | An anonymous function definition expression is returned
exec = exec' f
  where
    f = bare . Par . AnonFunc Nothing []

execCall = exec' f
  where
    f = bare . call0 . Par . AnonFunc Nothing []

noCrawling :: API m a => m URL
noCrawling = pin "robots.txt" $ return $ Prelude.const $ return $ WR.noRobots

--

(/) :: URL.URL -> TS.Text -> URL.URL
url / tail = url & URL.segments <>~ [tail]


redirectToHttps :: URL -> IO ()
redirectToHttps url =
  void $ forkIO $ Warp.runSettings settings
    $ \_ respond -> redirect url & HR.toRaw & respond
  where
    settings = Warp.setPort 80 Warp.defaultSettings
