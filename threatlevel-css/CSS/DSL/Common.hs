module CSS.DSL.Common where

import Common.Prelude
import qualified Data.Text as TS
import CSS.Syntax

-- | Turn function on @SimpleSelector@ to function on @Selector@
toSS :: (SimpleSelector -> SimpleSelector) -> Selector -> Selector
toSS f = \case
  Simple ss -> Simple (f ss)
  Combined op s ss -> Combined op s (f ss)

mkSelectorMod
  :: ASetter' SimpleSelector [a]
  -> (TS.Text -> Maybe TS.Text -> a)
  -> TS.Text -> Selector -> Selector
mkSelectorMod lens syntax name = toSS (lens %~ (syntax name Nothing:))

mkSelectorModArg
  :: ASetter' SimpleSelector [a]
  -> (TS.Text -> Maybe TS.Text -> a)
  -> TS.Text -> TS.Text -> Selector -> Selector
mkSelectorModArg lens syntax name value = toSS (lens %~ (syntax name (Just value):))
