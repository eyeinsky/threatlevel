module Web.DSL.Helpers where

import Common.Prelude
import CSS
import XML


styled :: (CSS m, Exclamatable a Id) => a -> CSSM -> m a
styled elem rules = do
  id <- cssId rules
  return $ elem ! id

styleds :: (CSS m, Exclamatable a Class) => a -> CSSM -> m a
styleds elem rules = do
  class_ <- css rules
  return $ elem ! class_
