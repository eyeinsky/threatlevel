module Server.Response where

import qualified Prelude as P
import Common.Prelude
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified Network.HTTP.Types as WT
import qualified Network.Wai as Wai
import qualified Data.Aeson as Aeson
import qualified Data.Text.Lazy.Lens as LL
import qualified Data.Text.Strict.Lens as SL

import Network.HTTP.Types

-- import qualified JS
-- import qualified JS.Syntax
import qualified HTML
import Render
import URL

import HTML (Html, Document(Document))

import qualified Network.WebSockets as WS
