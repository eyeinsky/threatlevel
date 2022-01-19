{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module JS.DSL.Polysemy
  ( module JS.DSL.Polysemy
  , module JS.DSL.Polysemy.Base
  , module JS.DSL.Syntax
  , module Syntax

  -- , M
  -- , run, runEmpty, JS.State(..), askEnv
  -- , mkCode, mkCode_, next
  -- , Function, func
  -- , library
  ) where

import qualified Prelude as P
import Common.Prelude as P hiding (break, next)
import qualified Data.Set as S
import qualified Data.Hashable as H
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Either
import Control.Monad.Writer (WriterT, execWriterT)

import qualified Control.Monad.Reader

import Render

import JS.Syntax as Syntax
import JS.DSL.Syntax
import JS.DSL.Polysemy.Function
import JS.DSL.Polysemy.Base

-- @Constraint@
type Cc :: EffectRow -> EffectRow -> Constraint
type Cc s r = (Member (JS s) r, r ~ (JS s : s))

-- @Type@ with explicit @r@
type Csr :: EffectRow -> EffectRow -> Type -> Type
type Csr s r a = Cc s r => Sem r a -- :: Type

-- with @forall r@
type C :: EffectRow -> Type -> Type
type C s a = forall r . (Member (JS s) r, r ~ (JS s : s)) => Sem r a

-- * Variable

-- ** Declaration

bind
  :: forall s a . (VarDecl a ()) -> Expr a -> Name -> C s (Expr a)
bind syntax expr name = do
  emitStatement $ syntax name expr
  return (EName name)

declare
  :: forall s a
   . (VarDecl a ()) -> Expr a -> C s (Expr a)
declare syntax expr = bind @s syntax expr =<< getFreshIdentifier @s

let_, const, var :: forall s a . Expr a -> C s (Expr a)
var = declare @s Syntax.VarDef
let_ = declare @s Syntax.Let
const = declare @s Syntax.Const

bare :: forall s a . Expr a -> C s ()
bare e = emitStatement $ BareExpr e

-- ** Assignment

-- -- | Shorthands for assignment statement
infixr 4 .=
(.=) :: forall s a b. Expr a -> Expr b -> C s ()
lhs .= rhs = emitStatement @s $ BareExpr $ lhs `Assign` rhs

-- | @infixr 0@ shorthand for assignment statement -- for combining
-- with the dollar operator (@$@).
(.=$) :: forall s a b . Expr a -> Expr b -> C s ()
(.=$) = (.=) @s
infixr 0 .=$

-- | Compound assignments in statement form
(.+=), (.-=), (.*=), (./=) :: Expr b -> Expr b -> C s ()
a .+= b = (.=) a (a + b)
a .-= b = (.=) a (a - b)
a .*= b = (.=) a (a * b)
a ./= b = (.=) a (a P./ b)

-- * Comment

-- | Stopgap until syntax for block and single-line comments
comment :: forall s . TS.Text -> C s ()
comment text = bare @s $ ex $ "// " <> text

-- * Control flow

ifMaybeElse
  :: forall s r
   . (Cc s r)
  => Expr Bool -> Sem r () -> Maybe (Sem r ()) -> Sem r ()
ifMaybeElse cond true maybeFalse = emitStatement =<<$ IfElse cond
  <$> generateCode true
  <*> traverse generateCode maybeFalse

ifelse :: Expr Bool -> C s () -> C s () -> C s ()
ifelse c t e = ifMaybeElse c t (Just e)

ifonly :: Expr Bool -> C s () -> C s ()
ifonly c t = ifMaybeElse c t Nothing

return_ :: forall s a . Expr a -> C s ()
return_ e = emitStatement $ Return $ Cast e

retrn :: forall s a . Expr a -> C s ()
retrn = return_
{-# DEPRECATED retrn "Use return_ instead" #-}

empty :: C s ()
empty = emitStatement Empty

-- * Try/catch

tryCatch :: forall s a . C s () -> (Expr a -> C s ()) -> C s ()
tryCatch try catch = do
  try' <- generateCode try
  err <- getFreshIdentifier @s
  catch' <- generateCode $ catch (EName err)
  emitStatement $ TryCatchFinally try' [(err, catch')] Nothing

tryCatchFinally :: forall s a . C s () -> (Expr a -> C s ()) -> C s () -> C s ()
tryCatchFinally try catch finally = do
  try' <- generateCode try
  err <- getFreshIdentifier @s
  catch' <- generateCode $ catch (EName err)
  finally' <- generateCode finally
  emitStatement $ TryCatchFinally try' [(err, catch')] (Just finally')

throw :: forall s a. Expr a -> C s ()
throw e = emitStatement @s $ Throw e

-- * Swtich

-- -- m = match type
-- -- r = code block return type
-- type Case m r = Either (Code r) (Expr m, Code r)
-- type SwitchBodyM m r = WriterT [Case m r] (M r)

-- switch :: forall m r a. Expr m -> SwitchBodyM m r a -> M r ()
-- switch e m = do
--   li :: [Case m r] <- execWriterT m
--   let (def, cases') = partitionEithers li
--   write $ Switch e cases' (case def of def' : _ -> Just def'; _ -> Nothing)

-- case_ :: forall r m a . Expr m -> M r a -> SwitchBodyM m r ()
-- case_ match code = do
--   code' <- lift $ generateCode (code >> break)
--   tell $ pure $ Right (match, code')

-- default_ :: forall r a m . M r a -> SwitchBodyM m r ()
-- default_ code = lift (generateCode code) >>= Left ^ pure ^ tell

-- -- * Class

-- type ClassBodyM = forall r. WriterT [ClassBodyPart] (M r) ()

-- -- ** Class declaration

-- class_ :: Name -> ClassBodyM -> M r (Expr b)
-- class_ name bodyParts = classExtends name Nothing bodyParts

-- newClass :: ClassBodyM -> M r (Expr b)
-- newClass bodyParts = do
--   name <- next
--   class_ name bodyParts

-- classExtends :: forall r a. Name -> Maybe Name -> ClassBodyM -> M r (Expr a)
-- classExtends name what bodyParts = do
--   let uppercaseFirst :: TS.Text -> TS.Text
--       uppercaseFirst text = case TS.splitAt 1 text of
--         (c, hars) -> TS.toUpper c <> hars
--       name' = name & coerced %~ uppercaseFirst :: Name
--   bodyParts' <- execWriterT bodyParts
--   write @r $ Class name' what bodyParts'
--   return $ EName name'

-- -- ** Method and field helpers

-- constructor :: Function fexp => fexp -> ClassBodyM
-- constructor fexp = do
--   (formalArgs, functionBody) <- lift $ bla fexp
--   tell [ClassBodyMethod (Constructor formalArgs) functionBody]

-- methodMaker :: Function fexp => (Name -> [Name] -> ClassBodyMethodType) -> Name -> fexp -> ClassBodyM
-- methodMaker mm name fexp = do
--   (formalArgs, functionBody) <- lift $ bla fexp
--   tell [ClassBodyMethod (mm name formalArgs) functionBody]
--   return ()

-- method, staticMethod, get, staticGet, set  :: Function fexp => Name -> fexp -> ClassBodyM
-- method = methodMaker InstanceMethod
-- staticMethod = methodMaker StaticMethod
-- get = methodMaker (\a _ -> Getter a)
-- staticGet = methodMaker (\a _ -> StaticGetter a)
-- set = methodMaker (\a [b] -> Setter a b)

-- * Loops

break :: C s ()
break = emitStatement $ Break Nothing

continue :: C s ()
continue = emitStatement $ Continue Nothing

for :: forall s . Expr Bool -> C s () -> C s ()
for cond code = emitStatement . f =<< generateCode code
   where f = For Empty cond Empty
{-# DEPRECATED for "Use while instead. for needs a more percise implementation" #-}

while :: forall s . Expr Bool -> C s () -> C s ()
while cond code = emitStatement . While cond =<< generateCode code

mkForSyntax
  :: forall s a
   . (Name -> Expr a -> Code () -> Statement ())
  -> Expr a -> (Expr a -> C s ()) -> C s ()
mkForSyntax syntax expr mkBlock = do
   name <- getFreshIdentifier @s
   block <- generateCode $ mkBlock (EName name)
   emitStatement $ syntax name expr block
forIn, forAwait, forOf :: forall s a . Expr a -> (Expr a -> C s ()) -> C s ()
forIn = mkForSyntax ForIn
forAwait = mkForSyntax ForAwait
forOf = mkForSyntax ForOf

-- * Async/await

-- type Promise = Expr

-- await :: forall r m a . Expr a -> Poly r m (Expr a)
-- await = let_ @r . Syntax.Await
-- {-# DEPRECATED await "Use const $ Await instead." #-}

-- | Make a promise from function through @async@
promise
  :: forall f s a
   . (Function f, Row f ~ (JS s : s))
  => f -> C s (Expr a)
promise f = call0 <$> async f

-- * Typed functions

func, newf, async, generator
  :: forall s f
   . (Function f, Row f ~ (JS s : s))
  => f -> C s (Expr (ApplyType f))

func f = let_ =<< getSyntax AnonFunc f
newf = func
async f = let_ =<< getSyntax Async f
generator f = let_ =<< getSyntax Generator f

-- fn :: (Function f, Back (Expr (JS.Type f))) => f -> M r (Convert (Expr (JS.Type f)))
-- fn f = newf f <&> convert []
-- fn' n f = newf' n f <&> convert []

-- async_ :: (Function f, Back (Expr (JS.Type f))) => f -> M r (Convert (Expr (JS.Type f)))
-- async_ f = async f <&> convert []

-- -- * Modules

-- lib :: forall r m a . M r (Expr a) -> Poly r m (Expr a)
-- lib mcode = do
--   env <- askEnv
--   let
--     JS.State fresh used lib = def
--     codeText = render env $ resultCode $ run env lib used fresh $ mcode
--     codeHash = H.hash codeText
--     nameExpr = EName $ Name $ "h" <> TS.replace "-" "_" (TL.toStrict $ tshow codeHash)

--   set <- getLibrary
--   when (P.not $ codeHash `S.member` set) $ do
--     f <- mcode
--     nameExpr .= f
--     modifyLibrary (S.insert codeHash)
--   return nameExpr

-- -- * Convenience

-- -- | Runs code in another context with no ability to return
-- noReturn :: forall r m a. M Void a -> Poly r m ()
-- noReturn mcode = do
--   code :: Code Void <- mkCode @r @Void mcode
--   mapM_ (write @r . Syntax.NoReturn) code

-- -- * Convenience

printJS :: MonoJS a -> IO ()
printJS = TL.putStrLn . render (Syntax.Indent 2)

test :: C s ()
test = void $ do
  _ <- bind Syntax.VarDef Null "a"
  _ <- bind Syntax.Let Undefined "a"
  _ <- bind Syntax.Const Null "a"

  a <- var "a"
  b <- let_ "b"
  c <- const "c"

  a .= b

  comment "jee"

  ifelse (lit True) (do
    d <- let_ "d"
    b .= d)
    (do
    e <- let_ "e"
    c .= e)

  return_ $ lit 1

  tryCatchFinally
    (throw $ lit "error")
    (\e -> a .= e)
    (throw $ lit "finally")

  while (lit 2 .> 1) $ do
    bare $ call1 (ex "console" !. "log") a

  forIn (lit 1) $ \x -> do
    bare $ call1 (ex "console" !. "log") x

  JS.DSL.Polysemy.forOf (lit 1) $ \x -> do
    bare $ call1 (ex "console" !. "log") x

  log <- newf $ \a b -> do
    bare $ call1 (ex "console" !. "log") a
    bare $ call1 (ex "console" !. "log") b
    logAsync <- async $ \e ->
      bare $ call1 (ex "console" !. "log") e
    return_ logAsync

  logGen <- generator $ \e ->
    bare $ call1 (ex "console" !. "log") e

  bare $ call1 log 1
  bare $ call1 logGen 1

  a .+= b
  a ./= b

  retrn $ lit 1
  return ()

hot = printJS test
