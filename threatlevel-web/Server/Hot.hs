module Server.Hot where

import qualified Network.Wai.Handler.WarpTLS as Warp
import Rapid

import Common.Prelude
import qualified Server.API as API
import Server.Run
import Server.Wai

mkHot :: Ord k => k -> IO () -> (IO (), IO ())
mkHot name what = let
  reload = rapid 1 (\r -> restart r name what)
  stop_ = rapid 1 (\r -> stop r name what)
  in (reload, stop_)

type HotType k r a = (Ord k) => k -> SiteType r a

mkHotPrim :: Maybe Warp.TLSSettings -> HotType k r (IO (), IO (), IO ())
mkHotPrim maybeTls name mc ms siteRoot settings env site = (hot, stop, main)
  where
    main = siteMain' maybeTls mc ms siteRoot settings env site
    (hot, stop) = mkHot name main

hotHttp :: HotType k r (IO (), IO (), IO ())
hotHttp = mkHotPrim Nothing

hotHttps :: Warp.TLSSettings -> HotType k r (IO (), IO (), IO ())
hotHttps tls = mkHotPrim (Just tls)
