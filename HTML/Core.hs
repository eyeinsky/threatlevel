module HTML.Core
  ( module HTML.Core
  , module DOM.Core
  ) where

import Pr hiding (id)
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Monad.Writer

import qualified JS

import DOM.Core
import DOM.Event
import XML


data Attribute where
  Custom :: TL.Text -> TL.Text -> Attribute
  OnEvent :: Event event => event -> JS.Expr a -> Attribute
  Data ::  TL.Text -> TL.Text -> Attribute
  AttrClass :: [Class] -> Attribute
  AttrId :: Id -> Attribute

declareLenses [d|
   data HTML
      = TagNode {
           tagLens  :: TagName
         , id       :: Maybe Id
         , classes  :: [Class]
         , attrs    :: HM.HashMap TL.Text Attribute
         , contents :: [HTML]
         }
      | TextNode TL.Text
      | JSNode (JS.Expr DocumentFragment)
   |]

instance IsString HTML where
   fromString str = TextNode $ TL.pack str

instance IsString Html where
   fromString str = text $ TL.pack str

-- ** Shorthands

tag str = TagNode (TagName str) Nothing [] HM.empty []

-- ** Monadic dsl

type M = Writer [HTML]

type Html = M ()

text :: TL.Text -> Html
text tl = tell [TextNode tl]

dyn expr = tell [JSNode (JS.Cast expr)]

class Attributable a where
  (!) :: a -> Attribute -> a

instance Attributable HTML where
  (!) a attr = case attr of
    AttrClass cs -> a & classes %~ (cs <>)
    AttrId id' -> a & id .~ Just id'
    Custom k v -> a & attrs %~ (HM.insert k attr)
    OnEvent e v -> a & attrs %~ (HM.insert (toOn e) attr)
    Data e v -> a & attrs %~ (HM.insert e attr)

instance Attributable Html where
  (!) a attr = case execWriter a of
    e : rest -> tell (e ! attr : rest)
    _ -> return ()

instance Attributable (Html -> Html) where
  (!) a attr = case execWriter (a $ pure ()) of
    e : rest -> \ x -> let
        ct = execWriter x
        e' = e ! attr & contents .~ ct
      in tell (e' : rest)
    _ -> a

declareFields [d|
  data Document = Document
    { documentHead :: Html
    , documentBody :: Html
    }
  |]
