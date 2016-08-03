module Web.DOM.Event where

import Prelude2
import Text.Exts
import qualified Data.Text as T

class Show a => Event a where
instance Event MouseEvent
instance Event KeyboardEvent
instance Event HTMLFrameObjectEvent
instance Event HTMLFormEvent
instance Event Progress
instance Event Touch

data MouseEvent
   = Click
   | DblClick
   | MouseDown
   | MouseUp
   | MouseOver
   | MouseMove
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
   = HFE_Load
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
   | Load
   | LoadEnd

data Touch
   = TouchStart
   | TouchEnd
   | TouchMove
   | TouchEnter
   | TouchLeave
   | TouchCansel

deriving instance Show KeyboardEvent
deriving instance Show MouseEvent
deriving instance Show HTMLFrameObjectEvent
deriving instance Show HTMLFormEvent
deriving instance Show Progress
deriving instance Show Touch

-- | Make 'on$event' attribute
class Show a => ToOn a where
   toOn :: a -> T.Text
   toOn = ("on"<>) . T.toLower . tshow
instance ToOn MouseEvent
instance ToOn KeyboardEvent
instance ToOn HTMLFrameObjectEvent
instance ToOn HTMLFormEvent
