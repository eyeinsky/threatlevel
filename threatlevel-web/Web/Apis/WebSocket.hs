{-# LANGUAGE DeriveAnyClass #-}
module Web.Apis.WebSocket where

import Common.Prelude
import URL
import JS qualified
import JS hiding (Event)

-- * Client-side

data WebSocket

data WebSocketEvent
  = Open
  | Close

deriving instance Show WebSocketEvent
deriving instance JS.Event WebSocketEvent

wss :: URL -> URL
wss url = url & proto .~ "wss"

ws :: URL -> URL
ws url = url & proto .~ "ws"

doSend :: JS m => Expr WebSocket -> Expr a -> m ()
doSend ws data_ = bare $ ws !// "send" $ stringify data_

openSocket :: Expr URL -> [Expr String] -> Expr WebSocket
openSocket url protos = call (New (ex "WebSocket")) [url, Cast $ lit protos]
