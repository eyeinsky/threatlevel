{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
module JS.DSL.Polysemy
  ( module JS.DSL.Polysemy
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
import JS.DSL.Polysemy.Function as JS
import JS.DSL.Polysemy.Core as JS


-- runEmpty :: Syntax.Conf -> M r a -> Result r a
-- runEmpty env m = run env mempty mempty validIdentifiers m

-- -- -- * Variable

-- -- -- ** Declaration

-- bind
--   :: forall r m a b
--    . (Name -> Expr a -> Statement r) -> Expr a -> Name -> Poly r m (Expr b)
-- bind decl expr name = do
--   write $ decl name expr
--   return $ EName name

-- newPrim
--   :: forall r m a
--    . (Name -> Expr a -> Statement r) -> Expr a -> Poly r m (Expr a)
-- newPrim kw e = bind kw e =<< next

-- new, let_, const, var :: forall r m a . Expr a -> Poly r m (Expr a)
-- new e = newPrim @r VarDef e
-- {-# DEPRECATED new "Use const, let_ or var instead." #-}
-- var = new @r
-- let_ = newPrim @r Let
-- const = newPrim @r Const

-- new' :: forall r m a . TS.Text -> Expr a -> Poly r m (Expr a)
-- new' n e = bind @r Let e =<< pushName n

-- bare :: forall r m a . Expr a -> Poly r m ()
-- bare e  = write @r $ BareExpr e

-- block :: forall r m a . M r a -> Poly r m (Expr r)
-- block    = let_ @r <=< blockExpr

-- block' :: forall r m a . TS.Text -> M r a -> Poly r m (Expr r)
-- block' n = new' @r n <=< blockExpr

-- -- -- ** Assignment

-- -- | Shorthands for assignment statement
-- infixr 4 .=
-- (.=) :: forall r m a b. Expr a -> Expr b -> Poly r m ()
-- lhs .= rhs = write @r $ BareExpr $ lhs `Assign` rhs

-- -- | @infixr 0@ shorthand for assignment statement -- for combining
-- -- with the dollar operator (@$@).
-- (.=$) :: forall r m a b . Expr a -> Expr b -> Poly r m ()
-- (.=$) = (.=) @r
-- infixr 0 .=$

-- -- | Compound assignments in statement form
-- (.+=), (.-=), (.*=) :: forall r m a . Num (Expr a) => Expr a -> Expr a -> Poly r m ()
-- a .+= b = (.=) @r a (a + b)
-- a .-= b = (.=) @r a (a - b)
-- a .*= b = (.=) @r a (a * b)
-- (./=) :: forall r m a . Fractional (Expr a) => Expr a -> Expr a -> Poly r m ()
-- a ./= b = (.=) @r a (a P./ b)

-- -- -- * Comment

-- -- -- | Stopgap until syntax for block and single-line comments
-- comment :: forall r m . TS.Text -> Poly r m ()
-- comment text = bare @r $ ex $ "// " <> text

-- -- -- * Control flow

-- ifmelse :: forall r m a. Expr Bool -> M r a -> Maybe (M r a) -> Poly r m ()
-- ifmelse cond true mFalse = do
--    trueCode <- mkCode_ true
--    mElseCode <- maybe (return Nothing) (fmap Just . mkCode_) mFalse
--    write $ IfElse cond trueCode mElseCode

-- ifelse :: Expr Bool -> M r a -> M r a -> Poly r m ()
-- ifelse c t e = ifmelse c t (Just e)

-- ifonly :: Expr Bool -> M r a -> Poly r m ()
-- ifonly c t   = ifmelse c t Nothing

-- return_ :: forall r m . Expr r -> Poly r m ()
-- return_ e = write @r $ Return $ Cast e

-- retrn :: forall r m . Expr r -> Poly r m ()
-- retrn = return_

-- empty :: forall r m . Poly r m ()
-- empty = write @r Empty

-- -- -- * Try/catch

-- tryCatch :: forall r m a . M r () -> (Expr a -> M r ()) -> Poly r m ()
-- tryCatch try catch = do
--   try' <- mkCode_ try
--   err <- next
--   catch' <- mkCode_ $ catch (EName err)
--   write $ TryCatchFinally try' [(err, catch')] Nothing

-- tryCatchFinally :: forall r m a . M r () -> (Expr a -> M r ()) -> M r () -> Poly r m ()
-- tryCatchFinally try catch finally = do
--   try' <- mkCode_ try
--   err <- next
--   catch' <- mkCode_ $ catch (EName err)
--   finally' <- mkCode_ finally
--   write $ TryCatchFinally try' [(err, catch')] (Just finally')

-- throw :: forall r m a. Expr a -> Poly r m ()
-- throw e = write @r $ Throw e

-- -- * Swtich

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
--   code' <- lift $ mkCode_ (code >> break)
--   tell $ pure $ Right (match, code')

-- default_ :: forall r a m . M r a -> SwitchBodyM m r ()
-- default_ code = lift (mkCode_ code) >>= Left ^ pure ^ tell

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

-- -- * Loops

-- for :: forall r m a . Expr r -> M r a -> Poly r m ()
-- for cond code = write @r . f =<< mkCode_ code
--    where f = For Empty cond Empty

-- forIn :: forall r m a b. Expr a -> (Expr b -> M r ()) -> Poly r m ()
-- forIn expr mkBlock = do
--    name <- next
--    block <- mkCode_ $ mkBlock (EName name)
--    write @r $ ForIn name expr block

-- forAwait :: forall r m a b. Expr a -> (Expr b -> M r ()) -> Poly r m ()
-- forAwait expr mkBlock = do
--    name <- next
--    block <- mkCode_ $ mkBlock (EName name)
--    write @r $ ForAwait name expr block

-- forOf :: forall r m a b. Expr a -> (Expr b -> M r ()) -> Poly r m ()
-- forOf expr mkBlock = do
--    name <- next
--    block <- mkCode_ $ mkBlock (EName name)
--    write @r $ ForOf name expr block

-- while :: forall r m a . Expr r -> M r a -> Poly r m ()
-- while cond code = write @r . f =<< mkCode_ code
--    where f = While cond

-- break :: forall r m . Poly r m ()
-- break = write @r $ Break Nothing

-- continue :: forall r m . Poly r m ()
-- continue = write @r $ Continue Nothing

-- -- * Async/await

-- type Promise = Expr

-- await :: forall r m a . Expr a -> Poly r m (Expr a)
-- await = let_ @r . Syntax.Await
-- {-# DEPRECATED await "Use const $ Await instead." #-}

-- -- -- | Make a promise out of a function through async
-- promise :: forall r m f a . Function f => f -> Poly r m (Promise a)
-- promise f = call0 <$> async f

-- -- * Unsorted

-- blockExpr :: forall r m a . M r a -> Poly r m (Expr r)
-- blockExpr = fmap (AnonFunc Nothing []) . mkCode_
-- ^ Writes argument 'M r a' to writer and returns a callable name

-- -- * Typed functions

-- newf, async, generator :: forall r m f . Function f => f -> Poly r m (Expr (JS.Type f))
-- newf :: forall r m f . Function f => f -> Poly r m (Expr (JS.Type f))
-- newf f = do
  -- x <- func AnonFunc f
  -- let_ @r x -- <=<

-- test :: forall r m . Poly r m ()
-- test = do
--   embed (undefined :: M r ())

-- async = let_ @r <=< func Async
-- generator = let_ @r <=< func Generator

-- newf' :: Function f => TS.Text -> f -> M r (Expr (JS.Type f))
-- newf' n = new' n <=< func AnonFunc

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

-- instance Render (M r a) where
--   type Conf (M r a) = Syntax.Conf
--   renderM m = do
--     env <- Control.Monad.Reader.ask
--     renderM $ resultCode $ runEmpty env $ m

-- pr :: M r a -> IO ()
-- pr = TL.putStrLn . render (Syntax.Indent 2)
