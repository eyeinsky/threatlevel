{-# OPTIONS_GHC -Wno-orphans #-}
module Web.X
  ( module Web.X
  , module Export
  ) where

import HTML as Export hiding
  -- redefined here
  ( href, src, for, favicon, html, action, style
  -- used in URL
  , param,
  -- used in CSS
  em, font, content, Value,
  -- used in HTTP
  header,
  raw,
  -- used in JS.DSL
  var, method,
  -- conflict with DOM.Core
  Id, Class, Tag, Id, Class
  )

import CSS as Export hiding
  -- generic
  ( filter, all, not
  -- defined in HTML
  , disabled, readOnly, required, default_, scope, link, placeholder
  -- defined in JS
  , empty
  -- defined in URL
  , host
  --
  , Minify, execSub
  )
import JS.Event as Export
import Web.Apis.Event as Export

import URL as Export

import JS as Export hiding
  -- todo: describe these
  ( dir, for, Conf, String, State, concat
  )

import Web.Apis.DOM as Export
import Web.Apis.CustomElement as Export

import Common.Prelude
import qualified Data.Text as TS
import Data.Time

import qualified Prelude as P
import CSS qualified

import qualified HTML
import DOM.Core qualified as DOM

-- * HTML + Date.Time

parseTextTime :: (Monad m, MonadFail m, ParseTime t) => String -> TS.Text -> m t
parseTextTime fmt inp =
  parseTimeM True defaultTimeLocale fmt str
  where
    str = TS.unpack inp

-- * JS + CSS

instance ToExpr DOM.Id where
  lit = coerce @_ @DOM.Value ^ render' ^ lit

instance ToExpr DOM.Class where
  lit = coerce @_ @DOM.Value ^ render' ^ lit

instance ToExpr SimpleSelector where
  lit = lit . render CSS.Minify

-- | In JS set element's inline style to @declarations@ [api]
inlineStyle :: JS m => Expr tag -> MonoProp () -> m ()
inlineStyle element declarations = do
  forM_ (execMonoProp declarations) $ \(Declaration k v) -> let
    property = tsKebab2camel k
    in element !. "style" !. property .= lit (render CSS.Minify v)

-- * JS + HTML + URL

jsHref :: Expr a -> Attribute
jsHref url = HTML.href (Dynamic $ Cast url)

-- * Date

-- | Formats dates as "in 2 days" etc.
--
-- Adapted from here: https://blog.webdevsimplified.com/2020-07/relative-time-format/
mkFormatFromNow
  :: ( JS m
     , Back (Expr (FunctionType (m ()) m))
     , Convert (Expr (FunctionType (m ()) m)) ~ Expr String
     ) =>
     Expr a1 -> m (Expr Date -> Expr String)
mkFormatFromNow formatter = do
  divisions <- const (lit $ map (\(a, b) -> lit [a, b])
    [ (60, "seconds")
    , (60, "minutes")
    , (24, "hours")
    , (7, "days")
    , (4.34524, "weeks")
    , (12, "months")
    , (ex "Number" !. "POSITIVE_INFINITY", "years")
    ] :: Expr [(Double, String)])
  let amount a = a !- 0
      name a = a !- 1

  fn $ \(date :: Expr Date) -> do
    duration <- let_ $ (date - call0 (New dateConstructor)) P./ 1000
    iterArray divisions $ \ix -> do
      division <- const $ divisions !- ix
      ifonly ((ex "Math" !// "abs" $ duration) .< amount division) $ do
        return_ $ call (formatter !. "format")
          [ ex "Math" !// "round" $ duration
          , name division ]
      duration ./= amount division
    return_ (Null :: Expr String)
