module DOM.Event where

import X.Prelude
import JS.Event

instance Event MouseEvent
instance Event KeyboardEvent
instance Event HTMLFrameObjectEvent
instance Event HTMLFormEvent
instance Event HashChangeEvent
instance Event Progress
instance Event Touch
instance Event WorkerEvent
instance Event ServiceWorkerEvent
instance Event NetworkEvent

data MouseEvent
   = Click
   | DblClick
   | MouseDown
   | MouseUp
   | MouseOver
   | MouseMove
   | MouseEnter
   | MouseOut
   | DragStart
   | Drag
   | DragEnter
   | DragLeave
   | DragOver
   | Drop
   | DragEnd

data KeyboardEvent
   = KeyUp
   | KeyDown
   | KeyPress

-- HTML frame/object
data HTMLFrameObjectEvent
   = Load
   | Unload
   | Abort
   | Error
   | Resize
   | Scroll

data HTMLFormEvent
   = Select
   | Change
   | Submit
   | Reset
   | Focus
   | Blur

   -- | Fires on <input>, <select>, <textarea>, or where
   -- `contenteditable` or `designMode` are turned one
   | Input

data HashChangeEvent = HashChange

-- No "on" prefixed attributes for html elements these events:

data UIEvent
   = FocusIn
   | FocusOut
   | DOMActivate

{-  -- (not well supported per wikipedia, so won't implement
data DOMMutation
   | DOMSubtreeModified
   | DOMNodeInserted
   | DOMNodeRemoved
   | DOMNodeRemovedFromDocument
   | DOMNodeInsertedIntoDocument
   | DOMAttrModified
   | DOMCharacterDataModified
   -}

data Progress
   = LoadStart
   | Progress
   | Progress_Error
   | Progress_Abort
   | Load_
   | LoadEnd

data Touch
   = TouchStart
   | TouchEnd
   | TouchMove
   | TouchEnter
   | TouchLeave
   | TouchCansel

data WorkerEvent
  = Message

data ServiceWorkerEvent
  = Install
  | Activate
  | Fetch
  | Push
  | NotificationClick
  | Sync

data NetworkEvent
  = Offline
  | Online

deriving instance Show KeyboardEvent
deriving instance Show MouseEvent
deriving instance Show HTMLFrameObjectEvent
deriving instance Show HTMLFormEvent
deriving instance Show HashChangeEvent
deriving instance Show Progress
deriving instance Show Touch
deriving instance Show WorkerEvent
deriving instance Show ServiceWorkerEvent
deriving instance Show NetworkEvent
