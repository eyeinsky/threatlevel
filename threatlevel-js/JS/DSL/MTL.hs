{-# LANGUAGE ExtendedDefaultRules #-}
module JS.DSL.MTL
  ( module JS.DSL.MTL
  , module JS
  -- , M
  -- , run, runEmpty, State(..), askEnv
  -- , mkCode, mkCode_, next
  -- , Function, func, Final
  -- , library
  ) where

import qualified Prelude as P
import Common.Prelude as P hiding (break, next)
import qualified Data.Text as TS

import Data.Either
import Control.Monad.Writer

import JS.Syntax as Syntax
import JS.DSL.MTL.Effect as JS

-- * Variable

-- ** Declaration

let_, const, var :: JS m => Expr a -> m (Expr a)
var = bind VarDef
let_ = bind Let
const = bind Const

bare :: JS m => Expr a -> m ()
bare = stm . BareExpr

-- ** Assignment

-- | Shorthands for assignment statement
infixr 4 .=
(.=) :: JS m => Expr a -> Expr b -> m ()
lhs .= rhs = stm $ BareExpr $ lhs `Assign` rhs

-- | @infixr 0@ shorthand for assignment statement -- for combining
-- with the dollar operator (@$@).
(.=$) :: JS m => Expr a -> Expr b -> m ()
(.=$) = (.=)
infixr 0 .=$

-- | Compound assignments in statement form
(.+=), (.-=), (.*=) :: JS m => Num (Expr b) => Expr b -> Expr b -> m ()
a .+= b = a .= (a + b)
a .-= b = a .= (a - b)
a .*= b = a .= (a * b)
(./=) :: JS m => Fractional (Expr a) => Expr a -> Expr a -> m ()
a ./= b = a .= (a P./ b)

-- * Comment

-- | Stopgap until syntax for block and single-line comments
comment :: JS m => TS.Text -> m ()
comment text = bare $ ex $ "// " <> text

-- * Control flow

ifmelse :: JS m => Expr Bool -> m a -> Maybe (m a) -> m ()
ifmelse cond true mFalse = do
   (_, trueCode) <- execSub true
   mElseCode <- maybe (return Nothing) (fmap (Just . snd) . execSub) mFalse
   stm $ IfElse cond trueCode mElseCode

ifelse :: JS m => Expr Bool -> m a -> m a -> m ()
ifelse c t e = ifmelse c t (Just e)

ifonly :: JS m => Expr Bool -> m a -> m ()
ifonly c t   = ifmelse c t Nothing

return_ :: JS m => Expr r -> m ()
return_ e = stm $ Return $ Cast e

empty :: JS m => m ()
empty = stm Empty

-- * Loops

mkLoop :: JS m => (Name -> l -> Code_ -> Statement ()) -> l -> (Expr a -> m b) -> m ()
mkLoop syntax loopable mkBlock = do
   name <- freshName
   block <- execSub_ $ mkBlock (EName name)
   stm $ syntax name loopable block

for :: JS m => Expr r -> m a -> m ()
for cond code = stm . f =<< execSub_ code
   where f = For Empty cond Empty

forIn :: JS m => Expr a -> (Expr b -> m ()) -> m ()
forIn = mkLoop ForIn

forAwait :: JS m => Expr a -> (Expr b -> m ()) -> m ()
forAwait = mkLoop ForAwait

forOf :: JS m => Expr a -> (Expr b -> m ()) -> m ()
forOf = mkLoop ForOf

while :: JS m => Expr r -> m a -> m ()
while cond code = stm . f =<< execSub_ code
   where f = While cond

break :: JS m => m ()
break = stm $ Break Nothing

continue :: JS m => m ()
continue = stm $ Continue Nothing

-- * Try/catch

tryCatch :: JS m => m () -> (Expr a -> m ()) -> m ()
tryCatch try catch = do
  try' <- execSub_ try
  err <- freshName
  catch' <- execSub_ $ catch (EName err)
  stm $ TryCatchFinally try' [(err, catch')] Nothing

tryCatchFinally :: JS m => m () -> (Expr a -> m ()) -> m () -> m ()
tryCatchFinally try catch finally = do
  try' <- execSub_ try
  err <- freshName
  catch' <- execSub_ $ catch (EName err)
  finally' <- execSub_ finally
  stm $ TryCatchFinally try' [(err, catch')] (Just finally')

throw :: JS m => Expr a -> m ()
throw e = stm $ Throw e

-- * Swtich

-- m = match type
-- r = code block return type
type Case a = Either Code_ (Expr a, Code_)
type SwitchBodyM m a = WriterT [Case a] m ()

switch :: forall m a . JS m => Expr a -> SwitchBodyM m a -> m ()
switch e m = do
  li :: [Case a] <- execWriterT m
  let (def, cases') = partitionEithers li
  stm $ Switch e cases' (case def of def' : _ -> Just def'; _ -> Nothing)

case_ :: JS m => Expr a -> m b -> SwitchBodyM m a
case_ match code = do
  code' <- lift $ execSub_ (code >> break)
  tell $ pure $ Right (match, code')

default_ :: JS m => m a -> SwitchBodyM m b
default_ code = lift (execSub_ code) >>= Left ^ pure ^ tell

-- * Class

type ClassBodyM m = WriterT [ClassBodyPart] m ()

-- ** Class declaration

class_ :: JS m => Name -> ClassBodyM m -> m (Expr a)
class_ name bodyParts = classExtends name Nothing bodyParts

newClass :: JS m => ClassBodyM m -> m (Expr a)
newClass bodyParts = freshName >>= flip class_ bodyParts

classExtends :: JS m => Name -> Maybe Name -> ClassBodyM m -> m (Expr a)
classExtends name what bodyParts = do
  let uppercaseFirst :: TS.Text -> TS.Text
      uppercaseFirst text = case TS.splitAt 1 text of
        (c, hars) -> TS.toUpper c <> hars
      name' = name & coerced %~ uppercaseFirst :: Name
  bodyParts' <- execWriterT bodyParts
  stm $ Class name' what bodyParts'
  return $ EName name'

-- ** Method and field helpers

constructor :: JS m => Function f m => f -> ClassBodyM m
constructor f = do
  (formalArgs, functionBody) <- lift $ funcUntyped f
  tell [ClassBodyMethod (Constructor formalArgs) functionBody]

methodMaker
  :: forall m f . JS m => Function f m
  => (Name -> [Name] -> ClassBodyMethodType) -> Name -> f -> ClassBodyM m
methodMaker mm name f = do
  (formalArgs, functionBody) <- lift $ funcUntyped f
  tell [ClassBodyMethod (mm name formalArgs) functionBody]

method, staticMethod, get, staticGet, set
  :: JS m => Function f m
  => Name -> f -> ClassBodyM m
method = methodMaker InstanceMethod
staticMethod = methodMaker StaticMethod
get = methodMaker (\a _ -> Getter a)
staticGet = methodMaker (\a _ -> StaticGetter a)
set = methodMaker (\a [b] -> Setter a b)

-- * Functions

newf, async, generator :: JS m => Function f m => f -> m (Expr (FunctionType f m))
newf = let_ <=< func (\_ -> FuncArrow)
async = let_ <=< func (\_ -> AsyncArrow)
generator = let_ <=< func Generator

fn
  :: JS m => Function f m => Back (Expr (FunctionType f m))
  => f -> m (Convert (Expr (FunctionType f m)))
fn f = newf f <&> convert []

async_
  :: (JS m, Function f m, Back (Expr (FunctionType f m)))
  => f -> m (Convert (Expr (FunctionType f m)))
async_ f = async f <&> convert []

-- * Async/await

type Promise = Expr

-- | Make a promise out of a function through async
promise :: JS m => Function f m => f -> m (Promise b)
promise f = call0 <$> async f
