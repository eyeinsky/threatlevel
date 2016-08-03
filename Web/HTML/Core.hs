module Web.HTML.Core where

import Prelude2
import Text.Exts
import Data.String

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import qualified Data.HashMap.Strict as HM

-- * Base types

data TagName = TagName { unTagName :: TL.Text }
data Id      = Id { unId :: TL.Text }
data Class   = Class { unClass :: TL.Text }
deriving instance Show TagName
deriving instance Show Id
deriving instance Show Class

declareLenses [d|
   data HTML
      = TagNode {
           tagLens  :: TagName
         , id       :: Maybe Id
         , classes  :: [Class]
         , attrs    :: HM.HashMap TL.Text TL.Text
         , contents :: [HTML]
         }
      | TextNode TL.Text
   |]

instance IsString HTML where
   fromString str = TextNode $ TL.pack str

-- | Stubs
data Tag
data Attr
data Window
data Document
data Location

-- ** Shorthands

tag str = TagNode (TagName str) Nothing [] HM.empty []

