{-# LANGUAGE ExtendedDefaultRules #-}
module JS.DSL.MTL
  ( module JS.DSL.MTL
  , M, State(..), run
  , library, Function, funcPure, func, mkCode, Final, noReturn, next
  , pr
  ) where

import qualified Prelude as P
import Common.Prelude as P hiding (break)
import qualified Data.Set as S
import qualified Data.Hashable as H
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Either
import Control.Monad.Writer
import Control.Monad.State hiding (State)
import Control.Monad.Reader

import Render

import JS.Syntax hiding (Conf, Static)
import qualified JS.Syntax as Syntax
import JS.DSL.MTL.Function as JS
import JS.DSL.MTL.Core as JS

-- * Variable

-- ** Declaration

bind :: forall a b r. (Name -> Expr a -> Statement r) -> Expr a -> Name -> M r (Expr b)
bind decl expr name = do
  write $ decl name expr
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

bare :: Expr a -> M r ()
bare e  = write $ BareExpr e

block :: M r a -> M r (Expr r)
block    = let_    <=< blockExpr

block' :: TS.Text -> M r a -> M r (Expr r)
block' n = new' n <=< blockExpr

-- ** Assignment

-- | Shorthands for assignment statement
infixr 4 .=
(.=) :: Expr a -> Expr b -> M r ()
lhs .= rhs = write $ BareExpr $ lhs `Assign` rhs

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

ifmelse :: Expr Bool -> M r a -> Maybe (M r a) -> M r ()
ifmelse cond true mFalse = do
   trueCode <- mkCode true
   mElseCode <- maybe (return Nothing) (fmap Just . mkCode) mFalse
   write $ IfElse cond trueCode mElseCode

ifelse :: Expr Bool -> M r a -> M r a -> M r ()
ifelse c t e = ifmelse c t (Just e)

ifonly :: Expr Bool -> M r a -> M r ()
ifonly c t   = ifmelse c t Nothing

return_ :: Expr a -> M a ()
return_ e = write $ Return $ Cast e

retrn :: Expr a -> M a ()
retrn = return_

empty :: M a ()
empty = write Empty

-- * Try/catch

tryCatch :: M r () -> (Expr n -> M r ()) -> M r ()
tryCatch try catch = do
  try' <- mkCode try
  err <- next
  catch' <- mkCode $ catch (EName err)
  write $ TryCatchFinally try' [(err, catch')] Nothing

tryCatchFinally :: M r () -> (Expr n -> M r ()) -> M r () -> M r ()
tryCatchFinally try catch finally = do
  try' <- mkCode try
  err <- next
  catch' <- mkCode $ catch (EName err)
  finally' <- mkCode finally
  write $ TryCatchFinally try' [(err, catch')] (Just finally')

throw :: Expr a -> M r ()
throw e = write $ Throw e

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

case_ :: Expr m -> M r a -> SwitchBodyM m r ()
case_ match code = do
  code' <- lift $ mkCode (code >> break)
  tell $ pure $ Right (match, code')

default_ :: M r a -> SwitchBodyM m r ()
default_ code = lift (mkCode code) >>= Left ^ pure ^ tell

-- * Class

type ClassBodyM = forall r. WriterT [ClassBodyPart] (M r) ()

-- ** Class declaration

class_ :: Name -> ClassBodyM -> M r (Expr b)
class_ name bodyParts = classExtends name Nothing bodyParts

newClass :: ClassBodyM -> M r (Expr b)
newClass bodyParts = do
  name <- next
  class_ name bodyParts

classExtends :: Name -> Maybe Name -> ClassBodyM -> M r (Expr b)
classExtends name what bodyParts = do
  let uppercaseFirst :: TS.Text -> TS.Text
      uppercaseFirst text = case TS.splitAt 1 text of
        (c, hars) -> TS.toUpper c <> hars
      name' = name & coerced %~ uppercaseFirst :: Name
  bodyParts' <- execWriterT bodyParts
  write $ Class name' what bodyParts'
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

for :: Expr r -> M r a -> M r ()
for cond code = write . f =<< mkCode code
   where f = For Empty cond Empty

forIn :: Expr p -> (Expr n -> M r ()) -> M r ()
forIn expr mkBlock = do
   name <- next
   block <- mkCode $ mkBlock (EName name)
   write $ ForIn name expr block

forAwait :: Expr p -> (Expr n -> M r ()) -> M r ()
forAwait expr mkBlock = do
   name <- next
   block <- mkCode $ mkBlock (EName name)
   write $ ForAwait name expr block

forOf :: Expr p -> (Expr n -> M r ()) -> M r ()
forOf expr mkBlock = do
   name <- next
   block <- mkCode $ mkBlock (EName name)
   write $ ForOf name expr block

while :: Expr r -> M r a -> M r ()
while cond code = write . f =<< mkCode code
   where f = While cond

break :: M r ()
break = write $ Break Nothing

continue :: M r ()
continue = write $ Continue Nothing

-- * Async/await

type Promise = Expr

await :: Expr a -> JS.M r (Expr a)
await = let_ . Syntax.Await
{-# DEPRECATED await "Use const $ Await instead." #-}

-- | Make a promise out of a function through async
promise :: Function f => f -> JS.M r (Promise b)
promise f = call0 <$> async f

-- * Unsorted

blockExpr :: M r a -> M r (Expr r)
blockExpr = fmap (AnonFunc Nothing []) . mkCode
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
    State fresh used lib = def
    codeText = render env . snd . fst . run env fresh used lib $ mcode -- fix: take config from somewhere
    codeHash = H.hash codeText
    nameExpr = EName $ Name $ "h" <> TS.replace "-" "_" (TL.toStrict $ tshow codeHash)

  set <- gets (^.library)
  when (P.not $ codeHash `S.member` set) $ do
    f <- mcode
    nameExpr .= f
    modify (library %~ S.insert codeHash)
  return nameExpr
