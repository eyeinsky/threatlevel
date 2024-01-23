module Web
  ( module Export
  ) where

import JS as Export hiding
  ( Conf(..), empty, execSub, Env

  , split
  )

import CSS as Export hiding
  ( Conf(..), Reader_, State_, Writer_, empty, execSub
  , Tag --, Class, Id
  , runFresh, run

  , String, filter, not, all
  , placeholder -- HTML
  , required -- HTML
  , default_ -- JS
  , Value -- HTML
  , host -- URL
  )

import URL as Export

import Web.Prelude as Export hiding
  ( concat -- JS.BuiltIns.Array
  , not -- JS.DSL.Syntax
  , min -- JS.BuiltIns.Number
  , max -- JS.BuiltIns.Number
  , break  -- JS.MTL.DSL
  , last -- JS.BuiltIns.Array
  , first, forOf, set, aside, pre, (<+>)
  )

import Web.DSL as Export
import Web.DSL.Helpers as Export

-- | Pre
import Web.Lib.CSS_HTML as Export
import Web.Lib.HTML_URL as Export hiding (for)
import Web.Lib.URL_CSS as Export
import Web.Lib.JS_HTML as Export () -- it only contains instances
import Web.Lib.HTML as Export
import Web.Lib.JS_CSS as Export

import HTML as Export hiding
  ( src -- Web.Lib.HTML_URL
  , head -- Prelude
  , href -- Web.Lib.HTML_URL
  , for -- JS.DSL.MTL
  , favicon -- Web.Lib.HTML_URL
  , action -- Web.Lib.HTML_URL
  , style -- Web.Lib.CSS_HTML
  , var -- HTML.Core
  , dir -- HTML.Core
  , method -- HTML.Core
  , em -- CSS
  , font -- CSS
  , disabled -- CSS
  , readOnly -- CSS
  , content -- CSS
  , link -- CSS
  , scope  -- CSS
  , Class -- CSS
  , Id -- CSS
  , param -- URL
  , html -- Web.Lib.HTML
  )

-- * Web Apis

import Web.Apis.DOM as Export
-- import Web.Apis.Event as Export -- exported by Web.Apis.DOM
import Web.Apis.Fetch as Export
-- import Web.Apis.IDB as Export -- conflict: open
-- import Web.Apis.PWA as Export -- conflict: open
import Web.Apis.CustomElement as Export
import Web.Apis.WebSocket as Export
-- import Web.Apis.WebStorage as Export
