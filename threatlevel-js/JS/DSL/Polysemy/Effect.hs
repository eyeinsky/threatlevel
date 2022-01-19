{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy.Effect where

import Common.Prelude hiding (next)
import qualified Data.Text as TS

import Polysemy
import Polysemy.Internal (send)
import JS.Syntax as Syntax
import JS.DSL.Core hiding (State)
import qualified JS.DSL.Core as Core


data JS s :: Effect where
  GetFreshIdentifier :: JS s m Name
  GetPrefixedIdentifier :: TS.Text -> JS s m Name
  EmitStatement :: Statement () -> JS s m ()
  GenerateCode :: Sem (JS s : s) _a -> JS s m Code_

getFreshIdentifier :: forall s r. Member (JS s) r => Sem r Name
getFreshIdentifier = send (GetFreshIdentifier :: JS s (Sem r) Name)
{-# INLINABLE getFreshIdentifier #-}

getPrefixedIdentifier :: forall s r. Member (JS s) r => TS.Text -> Sem r Name
getPrefixedIdentifier prefix = send (GetPrefixedIdentifier prefix :: JS s (Sem r) Name)
{-# INLINABLE getPrefixedIdentifier #-}

emitStatement :: forall s r . (Member (JS s) r, r ~ (JS s : s)) => Statement () -> Sem r ()
emitStatement stm = send (EmitStatement stm :: JS s (Sem r) ())
{-# INLINABLE emitStatement #-}

generateCode :: forall s r a. Member (JS s) r => Sem (JS s : s) a -> Sem r Code_
generateCode body = send (GenerateCode body :: JS s (Sem r) Code_)
{-# INLINABLE generateCode #-}

-- makeSem ''JS
