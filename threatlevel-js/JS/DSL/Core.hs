module JS.DSL.Core where

import Prelude
import qualified Data.Text as TS
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HS

import Data.Default
import Control.Lens

import Identifiers
import qualified JS.Syntax as Syntax


type Idents = Infinite TS.Text
type Fresh = Idents
type Used = HS.HashMap TS.Text Idents
type Lib = S.Set Int
type Env = Syntax.Conf

data State = State
  { stateFreshIdentifiers :: Fresh
  , stateInUseIdentifiers :: Used
  , stateLibrary :: Lib
  }
makeFields ''State

instance Default State where
  def = State Syntax.validIdentifiers mempty S.empty

type Result r a = ((a, Syntax.Code r), State)

resultCode :: Result r a -> Syntax.Code r
resultCode result = code
  where ((_, code), _) = result
