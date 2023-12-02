module Web.DSL.Helpers where

import Common.Prelude
import CSS
import XML


styled :: (CSS m, Exclamatable a Class) => a -> m () -> m a
styled elem rules = do
  class_ <- css rules
  return $ elem ! class_

{-# DEPRECATED styleds "Use `styled` to get an element styled with a class, or `cssId`" #-}
styleds :: (CSS m, Exclamatable a Class) => a -> CSSM -> m a
styleds elem rules = do
  class_ <- css rules
  return $ elem ! class_
