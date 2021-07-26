module JS.DSL.MTL.Core where

import Prelude
import qualified Data.Text as TS
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HS
import Control.Monad.Writer
import Control.Monad.State hiding (State)
import Data.Default
import Control.Lens

import Render
import qualified Identifiers as I
import JS.Syntax.Reserved
import JS.Syntax hiding (Conf)

-- * Monad

type Idents = [TS.Text]
type Fresh = Idents
type Used = HS.HashMap TS.Text Idents
type Lib = S.Set Int

data State = State
  { stateFreshIdentifiers :: Fresh
  , stateInUseIdentifiers :: Used
  , stateLibrary :: Lib
  }
makeFields ''State

instance Default State where
  def = State idents mempty S.empty
    where
      idents = I.identifiersFilter (ecma1 <> ecma2 <> ecma5 <> ecma6)

type M r = WriterT (Code r) (StateT State Identity)

run :: Fresh -> Used -> Lib -> M r a -> ((a, Code r), State)
run fresh used lib m = id . st . wr $ m
   where id = runIdentity
         st = flip runStateT (State fresh used lib)
         wr = runWriterT

write :: Statement r -> M r ()
write stm = tell [stm]

-- * Core API

next :: M r Name
next = do
  name :: TS.Text <- I.next freshIdentifiers
  names <- gets (view inUseIdentifiers)
  if HS.member name names
    then next
    else return $ Name name

pushName :: TS.Text -> M r Name
pushName name = do
  names <- gets (view inUseIdentifiers)
  case HS.lookup name names of
    Just (s : uffixes) -> do
      modify (inUseIdentifiers %~ HS.insert name uffixes)
      return $ Name $ name <> "_" <> s
    _ -> do
      modify (inUseIdentifiers %~ HS.insert name I.identifierSource)
      return $ Name name

{- | Evaluate JSM code to @Code r@

     It doesn't actually write anything, just runs
     a JSM code into its code value starting from the
     next available name (Int) -- therefore not
     overwriting any previously defined variables. -}
mkCode :: M sub a -> M parent (Code sub)
mkCode mcode = do
  (w, s1) <- fromNext <$> get <*> pure mcode
  put s1 *> pure w
  where
    fromNext :: State -> M r t -> (Code r, State)
    fromNext (State fresh used lib) m = (w, s1)
      where
        ((_, w), s1) = run fresh used lib m

-- * Convenience

pr :: M r a -> IO ()
pr = TL.putStrLn . render (Indent 2) . snd . fst . run fresh used lib
  where
    State fresh used lib = def
