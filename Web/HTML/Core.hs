module Web.HTML.Core
  ( module Web.HTML.Core
  , execWriter
  ) where

import Prelude2 hiding (div, span, elem)
import Text.Exts
import Data.String

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import qualified Data.HashMap.Strict as HM

import Control.Monad.Writer

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

instance IsString (HTMLM ()) where
   fromString str = tell [fromString str]

-- | Stubs
data Tag
data Attr
data Window
data Document
data Location

-- ** Shorthands

tag str = TagNode (TagName str) Nothing [] HM.empty []

render :: HTML -> TL.Text
render html = case html of
  TagNode n mId cs as htmls ->
       "<"
       <> unTagName n
       <> (HM.null as' ? "" $ attrs)
    <> ">"
    <> TL.concat (map render htmls)
    <> "</" <> unTagName n <> ">"
    where
      as' = HM.union as (maybe HM.empty (HM.singleton "id" . unId) mId)
      q v = "'" <> v <> "'"
      attrs = HM.foldrWithKey (\k v x -> x <> " " <> k <> "=" <> q v) "" as'
  TextNode tl -> escape tl
    where escape tl = tl

-- ** Monadic dsl

type HTMLM = Writer [HTML] -- Identity

type Attribute = (TL.Text, TL.Text)

text :: TL.Text -> HTMLM ()
text tl = tell [TextNode tl]

class Attributable a where
  (!) :: a -> Attribute -> a

instance Attributable HTML where
  (!) a (k, v) = a & attrs %~ (HM.insert k v)

instance Attributable (HTMLM ()) where
  (!) a attr@ (k, v) = case execWriter a of
    e : rest -> tell (e ! attr : rest)
    _ -> return ()

instance Attributable (HTMLM () -> HTMLM ()) where
  (!) a attr@ (k, v) = case execWriter (a $ return ()) of
    e : rest -> \ x -> let
        ct = execWriter x
        e' = e ! attr & contents .~ ct
      in tell (e' : rest)
    _ -> a
