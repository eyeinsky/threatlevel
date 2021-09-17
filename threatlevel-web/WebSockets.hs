{-# LANGUAGE DeriveAnyClass #-}
module WebSockets
  ( module WebSockets
  ) where

import qualified Prelude as P
import Network.WebSockets as WS
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Aeson as A

import X.Prelude
import X hiding (Event)
import qualified X


data WebSocket

wss :: URL -> URL
wss url = url & proto .~ "wss"

ws :: URL -> URL
ws url = url & proto .~ "ws"

-- * Client-side

doSend :: Expr WebSocket -> Expr a -> M r ()
doSend ws data_ = bare $ ws !// "send" $ stringify data_

openSocket :: Expr URL -> [Expr String] -> Expr WebSocket
openSocket url protos = call (New (ex "WebSocket")) [url, Cast $ lit protos]

-- * Server-side

data Event
  = Open
  | Close

deriving instance Show Event
deriving instance X.Event Event

-- * Server

type WSMessage = Either WS.ControlMessage

class ToWSMessage a where
  wsMessage :: WS.Connection -> IO (Either WS.ControlMessage a)

instance ToWSMessage WS.DataMessage where
  wsMessage con = WS.receive con <&> \case
    WS.ControlMessage cm -> Left cm
    WS.DataMessage _ _ _ dm -> Right dm

instance ToWSMessage (Maybe A.Value) where
  wsMessage con = wsMessage con <&> either Left (Right . f)
    where
      f dm = case dm of
        WS.Text bs _ -> A.decode bs
        WS.Binary _ -> Nothing

instance ToWSMessage (Maybe TL.Text) where
  wsMessage con = wsMessage con <&> either Left (Right . f)
   where
     f = \case
       WS.Text bs _ -> bs
         & TL.decodeUtf8'
         & either (P.const Nothing) Just
       _ -> Nothing

instance ToWSMessage (Maybe TS.Text) where
  wsMessage con = wsMessage con <&> (fmap (fmap TL.toStrict))
