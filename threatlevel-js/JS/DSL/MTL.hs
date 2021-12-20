{-# LANGUAGE ExtendedDefaultRules #-}
module JS.DSL.MTL
  ( module JS.DSL.MTL
  , M
  , run, runEmpty, State(..), askEnv
  , mkCode, mkCode_, next
  , Function, func, Final
  , library
  ) where

import qualified Prelude as P
import Common.Prelude as P hiding (break, next)
import qualified Data.Set as S
import qualified Data.Hashable as H
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Either
import Control.Monad.Writer

import qualified Control.Monad.Reader

import Render

import JS.Syntax as Syntax
import JS.DSL.MTL.Function as JS
import JS.DSL.MTL.Core as JS


runEmpty :: Syntax.Conf -> M r a -> Result r a
runEmpty env m = run env mempty mempty validIdentifiers m

-- * Variable

-- ** Declaration

bind :: forall a b r. (Name -> Expr a -> Statement r) -> Expr a -> Name -> M r (Expr b)
bind decl expr name = do
  write @r $ decl name expr
  return $ EName name

newPrim :: (Name -> Expr a -> Statement r) -> Expr a -> M r (Expr a)
newPrim kw e = bind kw e =<< next

new, let_, const, var :: Expr a -> M r (Expr a)
new = newPrim VarDef
{-# DEPRECATED new "Use const, let_ or var instead." #-}
var = new
let_ = newPrim Let
const = newPrim Const

new' :: TS.Text -> Expr a -> M r (Expr a)
new' n e = bind Let e =<< pushName n

bare :: forall r a. Expr a -> M r ()
bare e  = write @r $ BareExpr e

block :: M r a -> M r (Expr r)
block    = let_    <=< blockExpr

block' :: TS.Text -> M r a -> M r (Expr r)
block' n = new' n <=< blockExpr

-- ** Assignment

-- | Shorthands for assignment statement
infixr 4 .=
(.=) :: forall r a b. Expr a -> Expr b -> M r ()
lhs .= rhs = write @r $ BareExpr $ lhs `Assign` rhs

-- | @infixr 0@ shorthand for assignment statement -- for combining
-- with the dollar operator (@$@).
(.=$) :: Expr a -> Expr b -> M r ()
(.=$) = (.=)
infixr 0 .=$

-- | Compound assignments in statement form
(.+=), (.-=), (.*=) :: Num (Expr b) => Expr b -> Expr b -> M r ()
a .+= b = a .= (a + b)
a .-= b = a .= (a - b)
a .*= b = a .= (a * b)
(./=) :: Fractional (Expr b) => Expr b -> Expr b -> M r ()
a ./= b = a .= (a P./ b)

-- * Comment

-- | Stopgap until syntax for block and single-line comments
comment :: TS.Text -> M r ()
comment text = bare $ ex $ "// " <> text

-- * Control flow

ifmelse :: forall r a. Expr Bool -> M r a -> Maybe (M r a) -> M r ()
ifmelse cond true mFalse = do
   trueCode <- mkCode_ true
   mElseCode <- maybe (return Nothing) (fmap Just . mkCode_) mFalse
   write $ IfElse cond trueCode mElseCode

ifelse :: Expr Bool -> M r a -> M r a -> M r ()
ifelse c t e = ifmelse c t (Just e)

ifonly :: Expr Bool -> M r a -> M r ()
ifonly c t   = ifmelse c t Nothing

return_ :: forall r. Expr r -> M r ()
return_ e = write @r $ Return $ Cast e

retrn :: Expr a -> M a ()
retrn = return_

empty :: forall r. M r ()
empty = write @r Empty

-- * Try/catch

tryCatch :: forall r a . M r () -> (Expr a -> M r ()) -> M r ()
tryCatch try catch = do
  try' <- mkCode_ try
  err <- next
  catch' <- mkCode_ $ catch (EName err)
  write $ TryCatchFinally try' [(err, catch')] Nothing

tryCatchFinally :: forall r a . M r () -> (Expr a -> M r ()) -> M r () -> M r ()
tryCatchFinally try catch finally = do
  try' <- mkCode_ try
  err <- next
  catch' <- mkCode_ $ catch (EName err)
  finally' <- mkCode_ finally
  write $ TryCatchFinally try' [(err, catch')] (Just finally')

throw :: forall r a. Expr a -> M r ()
throw e = write @r $ Throw e

-- * Swtich

-- m = match type
-- r = code block return type
type Case m r = Either (Code r) (Expr m, Code r)
type SwitchBodyM m r = WriterT [Case m r] (M r)

switch :: forall m r a. Expr m -> SwitchBodyM m r a -> M r ()
switch e m = do
  li :: [Case m r] <- execWriterT m
  let (def, cases') = partitionEithers li
  write $ Switch e cases' (case def of def' : _ -> Just def'; _ -> Nothing)

case_ :: forall r m a . Expr m -> M r a -> SwitchBodyM m r ()
case_ match code = do
  code' <- lift $ mkCode_ (code >> break)
  tell $ pure $ Right (match, code')

default_ :: forall r a m . M r a -> SwitchBodyM m r ()
default_ code = lift (mkCode_ code) >>= Left ^ pure ^ tell

-- * Class

type ClassBodyM = forall r. WriterT [ClassBodyPart] (M r) ()

-- ** Class declaration

class_ :: Name -> ClassBodyM -> M r (Expr b)
class_ name bodyParts = classExtends name Nothing bodyParts

newClass :: ClassBodyM -> M r (Expr b)
newClass bodyParts = do
  name <- next
  class_ name bodyParts

classExtends :: forall r a. Name -> Maybe Name -> ClassBodyM -> M r (Expr a)
classExtends name what bodyParts = do
  let uppercaseFirst :: TS.Text -> TS.Text
      uppercaseFirst text = case TS.splitAt 1 text of
        (c, hars) -> TS.toUpper c <> hars
      name' = name & coerced %~ uppercaseFirst :: Name
  bodyParts' <- execWriterT bodyParts
  write @r $ Class name' what bodyParts'
  return $ EName name'

-- ** Method and field helpers

constructor :: Function fexp => fexp -> ClassBodyM
constructor fexp = do
  (formalArgs, functionBody) <- lift $ bla fexp
  tell [ClassBodyMethod (Constructor formalArgs) functionBody]

methodMaker :: Function fexp => (Name -> [Name] -> ClassBodyMethodType) -> Name -> fexp -> ClassBodyM
methodMaker mm name fexp = do
  (formalArgs, functionBody) <- lift $ bla fexp
  tell [ClassBodyMethod (mm name formalArgs) functionBody]
  return ()

method, staticMethod, get, staticGet, set  :: Function fexp => Name -> fexp -> ClassBodyM
method = methodMaker InstanceMethod
staticMethod = methodMaker StaticMethod
get = methodMaker (\a _ -> Getter a)
staticGet = methodMaker (\a _ -> StaticGetter a)
set = methodMaker (\a [b] -> Setter a b)

-- * Loops

for :: forall r a. Expr r -> M r a -> M r ()
for cond code = write @r . f =<< mkCode_ code
   where f = For Empty cond Empty

forIn :: forall r a b. Expr a -> (Expr b -> M r ()) -> M r ()
forIn expr mkBlock = do
   name <- next
   block <- mkCode_ $ mkBlock (EName name)
   write @r $ ForIn name expr block

forAwait :: forall r a b. Expr a -> (Expr b -> M r ()) -> M r ()
forAwait expr mkBlock = do
   name <- next
   block <- mkCode_ $ mkBlock (EName name)
   write @r $ ForAwait name expr block

forOf :: forall r a b. Expr a -> (Expr b -> M r ()) -> M r ()
forOf expr mkBlock = do
   name <- next
   block <- mkCode_ $ mkBlock (EName name)
   write @r $ ForOf name expr block

while :: forall r a. Expr r -> M r a -> M r ()
while cond code = write @r . f =<< mkCode_ code
   where f = While cond

break :: forall r. M r ()
break = write @r $ Break Nothing

continue :: forall r. M r ()
continue = write @r $ Continue Nothing

-- * Async/await

type Promise = Expr

await :: Expr a -> JS.M r (Expr a)
await = let_ . Syntax.Await
{-# DEPRECATED await "Use const $ Await instead." #-}

-- -- | Make a promise out of a function through async
promise :: Function f => f -> JS.M r (Promise b)
promise f = call0 <$> async f

-- * Unsorted

blockExpr :: M r a -> M r (Expr r)
blockExpr = fmap (AnonFunc Nothing []) . mkCode_
-- ^ Writes argument 'M r a' to writer and returns a callable name

-- * Typed functions

newf, async, generator :: Function f => f -> M r (Expr (JS.Type f))
newf = let_ <=< func AnonFunc
async = let_ <=< func Async
generator = let_ <=< func Generator

newf' :: Function f => TS.Text -> f -> M r (Expr (JS.Type f))
newf' n = new' n <=< func AnonFunc

fn :: (Function f, Back (Expr (JS.Type f))) => f -> M r (Convert (Expr (JS.Type f)))
fn f = newf f <&> convert []
fn' n f = newf' n f <&> convert []

async_ :: (Function f, Back (Expr (JS.Type f))) => f -> M r (Convert (Expr (JS.Type f)))
async_ f = async f <&> convert []

-- * Modules

lib :: M r (Expr a) -> M r (Expr a)
lib mcode = do
  env <- ask
  let
    JS.State fresh used lib = def
    codeText = render env $ resultCode $ run env lib used fresh $ mcode
    codeHash = H.hash codeText
    nameExpr = EName $ Name $ "h" <> TS.replace "-" "_" (TL.toStrict $ tshow codeHash)

  set <- getLibrary
  when (P.not $ codeHash `S.member` set) $ do
    f <- mcode
    nameExpr .= f
    modifyLibrary (S.insert codeHash)
  return nameExpr

-- * Convenience

-- | Runs code in another context with no ability to return
noReturn :: forall r a. M Void a -> M r ()
noReturn mcode = do
  code :: Code Void <- mkCode @r @Void mcode
  mapM_ (write @r . Syntax.NoReturn) code

-- * Convenience

instance Render (M r a) where
  type Conf (M r a) = Syntax.Conf
  renderM m = do
    env <- Control.Monad.Reader.ask
    renderM $ resultCode $ runEmpty env $ m

pr :: M r a -> IO ()
pr = TL.putStrLn . render (Syntax.Indent 2)
