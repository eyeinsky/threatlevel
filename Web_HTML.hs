module Web_HTML where

import Prelude2

import qualified Data.Text.Lazy as TL

data Tag    = Tag TL.Text
data Id     = Id { unId :: TL.Text }
data Class  = Class { unClass :: TL.Text }
deriving instance Show Tag
deriving instance Show Id
deriving instance Show Class


