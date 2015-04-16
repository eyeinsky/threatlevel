module Web_HTML where

import Prelude2

import qualified Data.Text.Lazy as TL

data TagName = TagName { unTagName :: TL.Text }
data Id      = Id { unId :: TL.Text }
data Class   = Class { unClass :: TL.Text }
deriving instance Show TagName
deriving instance Show Id
deriving instance Show Class

data HTML
   = TagNode TagName (Maybe Id) [Class] [HTML]
   | TextNode TL.Text

data Attr
data Window

data KeyboardEvent 
   = KeyUp
   | KeyDown
   | KeyPress
   deriving (Show)


