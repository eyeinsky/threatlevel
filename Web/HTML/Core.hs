module Web.HTML.Core
  ( module Web.HTML.Core
  , module DOM.Internal
  ) where

import Prelude2 hiding (div, span, elem, id, text)
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Monad.Writer

import qualified JS

import DOM.Internal

-- | Stubs
data Tag
data Attr
data Window
data Document
data DocumentFragment
data Location

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
      | JSNode (JS.Expr DocumentFragment)
   |]

instance IsString HTML where
   fromString str = TextNode $ TL.pack str

instance IsString (HTMLM ()) where
   fromString str = text $ TL.pack str

-- ** Shorthands

tag str = TagNode (TagName str) Nothing [] HM.empty []

-- ** Monadic dsl

type HTMLM = Writer [HTML]

type Html = HTMLM ()

data Attribute
  = Custom TL.Text TL.Text
  | AttrClass [Class]
  | AttrId Id

text :: TL.Text -> HTMLM ()
text tl = tell [TextNode tl]

dyn expr = tell [JSNode (JS.Cast expr)]

class Attributable a where
  (!) :: a -> Attribute -> a

instance Attributable HTML where
  (!) a attr = case attr of
    AttrClass cs -> a & classes %~ (cs <>)
    AttrId id' -> a & id .~ Just id'
    Custom k v -> a & attrs %~ (HM.insert k v)

instance Attributable (HTMLM ()) where
  (!) a attr = case execWriter a of
    e : rest -> tell (e ! attr : rest)
    _ -> return ()

instance Attributable (HTMLM () -> HTMLM ()) where
  (!) a attr = case execWriter (a $ return ()) of
    e : rest -> \ x -> let
        ct = execWriter x
        e' = e ! attr & contents .~ ct
      in tell (e' : rest)
    _ -> a
