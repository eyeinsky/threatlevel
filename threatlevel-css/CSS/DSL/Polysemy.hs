module CSS.DSL.Polysemy where

import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.DList as DList

import X.Prelude hiding (State, Reader, Writer, runReader, runState, runWriter, tell, execWriter)

import Identifiers
import Polysemy hiding (run)
import qualified Polysemy
import Polysemy.State
import Polysemy.Reader
import Polysemy.Writer

import CSS.Syntax
import Render

-- tmp
import DOM.Core (Id(..))



-- * Types

type Rules = DList Rule
type Declarations = DList Declaration
type Names = Infinite TS.Text

-- * CSSM

-- ** Poly

type CurrentSelector = Reader Selector
type NamesState = State Names
type RulesW = Writer Rules
type DeclarationsW = Writer Declarations

type Poly m a =
  ( Member RulesW m
  , Member DeclarationsW m
  , Member NamesState m
  , Member CurrentSelector m
  ) => Sem m a

prop :: Member DeclarationsW m => TL.Text -> Value -> Sem m ()
prop k v = tell @Declarations $ pure $ mkDeclaration k v

rule
  :: ( Member DeclarationsW m'
     , Member RulesW m
     )
  => SelectorFrom s => s -> Sem m' a' -> Sem m ()
rule s ds = tell @Rules $ pure $ mkRule (selFrom s) $ undefined $ (runWriter ds :: _)

-- ** Fixed

type Fixed m a = Sem
  '[ RulesW
   , DeclarationsW
   , NamesState
   , CurrentSelector
   ] a

-- ~runCSSM
run :: Selector -> Infinite TS.Text -> Fixed m a -> (Names, (Declarations, (Rules, a)))
run r s m = m
  & runWriter
  & runWriter
  & runState s
  & runReader r
  & Polysemy.run

-- * DM

-- * TMP

test :: Declarations -- DList Declaration
test = decls
  where
  (names, (decls, (rules, ret))) = run (selFrom $ Id "bla") (toInfinite identifierSource) $ do
    prop "color" "red"

hot = render' $ DList.toList test
