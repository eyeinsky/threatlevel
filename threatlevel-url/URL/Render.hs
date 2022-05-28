{-# OPTIONS_GHC -Wno-orphans #-}
module URL.Render where

import Prelude
import Data.Coerce
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy as TL
import Control.Monad.Reader
import Network.HTTP.Types

import URL.Core
import Render

instance Render BaseURL where
  renderM (BaseURL proto host port) =
    renderM proto <+> pure "://" <+> renderM host <+> renderProtoPort proto port

withoutSchema :: BaseURL -> Reader () TL.Text
withoutSchema (BaseURL proto host port) =
  renderM host <+> renderProtoPort proto port

instance Render URL where
  renderM (URL proto (Authority auth host port) path params fragment) =
    renderM proto
    <+> pure "://"
    <+> pure (mkAuth auth)
    <+> renderM host
    <+> renderProtoPort proto port
    <+> renderM path
    <+> renderM params
    <+> renderM fragment

renderProtoPort :: Proto -> Port -> Reader () TL.Text
renderProtoPort (Proto proto') port
  | proto' == "http" && port == 80 = pure ""
  | proto' == "https" && port == 443 = pure ""
  | otherwise = pure ":" <+> renderM port

instance Render Authority where
  renderM (Authority auth host port) =
    pure (mkAuth auth) <+> renderM host <+> pure ":" <+> renderM port

mkAuth :: Maybe (TS.Text, TS.Text) -> TL.Text
mkAuth maybeAuth = case maybeAuth of
  Just (u, p) -> TL.fromStrict $ u <> ":" <> p <> "@"
  _ -> ""

instance Render Proto where
  renderM (Proto a) = pure $ TL.fromStrict a

instance Render Host where
  renderM h = pure $ case h of
    Domain t -> TL.fromStrict t
    IP4 a b c d -> TL.intercalate "." $ map (TL.pack . show) [a, b, c, d]

instance Render Port where
  renderM (Port w16) = pure $ TL.pack (show w16)

instance Render Path where
 renderM (Path p) = pure $ "/" <> TL.intercalate "/" (map (TL.fromStrict . coerce) p)
   -- renderM for Request => hast to start with /

instance Render Params where
  renderM (Params ps) = pure $ case ps of
    _ : _ -> "?" <> TL.intercalate "&" (map pair . coerce $ ps)
    _ -> ""
    where
      pair (a, b) = maybe a' (\b' -> a' <> "=" <> TL.fromStrict (urlEncode' b')) b
        where
          a' = TL.fromStrict a

instance Render Fragment where
  renderM (Fragment ts) = pure $ TL.fromStrict $ if TS.null ts
    then ""
    else "#" <> ts

urlEncode' :: TS.Text -> TS.Text
urlEncode' =  TS.decodeUtf8 . urlEncode True . TS.encodeUtf8
-- ^ True: encodes ","

renderURL :: URL -> TL.Text
renderURL url = render' url
