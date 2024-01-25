module CSS

  {-| The CSS package broadly has three kinds of content:

         1. MTL-based DSL, defined within "CSS.DSL"

         1. CSS properties (TH-generated) and combinators for the DSL. These are defined under "CSS" and "CSS.TH"

         1. CSS AST/syntax, defined within "CSS.Syntax"

  Users of the framework will probably want the first two, and use AST
  types only for corner cases not conveniently covered by the framework.

  CSS AST can also be used to create your own DSL.

  For the AST, there is only a printer available -- no parser.

  -}

  -- * Combinators
  ( descendant, child, sibling, generalSibling, tagSelector

  -- * Compound properties
  , centerContent, flexbox, square, circle

  -- * All CSS properties
  --
  -- | These are all TH-generated from CSS.TH.Generated.
  , module CSS.TH.Generated

  -- * Syntax
  , module CSS.Syntax
  , module CSS.DSL
  ) where

import Common.Prelude as P
import Data.Text qualified as TS
import CSS.Syntax hiding
  ( tag, maybeId, pseudos
  )
import CSS.DSL
import CSS.TH.Generated

descendant, child, sibling, generalSibling :: forall m a . (CSS m, SimpleSelectorFrom a) => a -> m () -> m ()
descendant = combinator Descendant
child = combinator Child
sibling = combinator Sibling
generalSibling = combinator GeneralSibling

tagSelector :: TS.Text -> Tag
tagSelector = Tag

centerContent :: Prop m => m ()
centerContent = do
  display "flex"
  flexFlow "column nowrap"
  justifyContent "center"
  alignItems "center"

flexbox :: Prop m => Value -> m ()
flexbox flow = do
  display "flex"
  flexFlow flow

square :: Prop m => Value -> m ()
square n = do
  width n
  height n

circle :: Prop m => Value -> m ()
circle n = do
  square n
  borderRadius $ prc 50
