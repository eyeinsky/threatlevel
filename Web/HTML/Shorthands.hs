module Web.HTML.Shorthands where

import Prelude2
import qualified Data.Text.Lazy as TL
import Control.Monad.Writer

import Language.Haskell.TH

import Web.HTML.Core

$(let mk name = [d| $(varP $ mkName name) = \ c -> tell [tag $(stringE name) & contents .~ execWriter (c :: HTMLM ())] :: HTMLM () |]
      -- mk name = [d| $(varP $ mkName name) = \ (c :: HTMLM ()) -> tell [tag $(stringE name) & contents .~ execWriter c] :: HTMLM () |]
  in concat <$> mapM mk ["div", "span", "a", "form", "h1", "ul", "li"])

$(let mk name = [d| $(varP $ mkName name) = \a -> ($(stringE name), a) :: Attribute |]
  in concat <$> mapM mk ["href"])

id_ :: Id -> Attribute
id_ (Id id) = ("id", id) :: Attribute

cls_ :: [Class] -> Attribute
cls_ li = ("className", TL.unwords $ map mk li) :: Attribute
  where
    mk (Class tl) = tl
