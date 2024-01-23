module Widgets.Menu where

import Common.Prelude
import Control.Monad.Writer
import Web.DSL.Helpers
import HTML qualified
import CSS
import Web

-- * DSL

data Menu a
  = Node a [Menu a]
  | Leaf a

type MenuM w a = WriterT w Identity a

runMenu :: WriterT w Identity a -> w
runMenu = execWriterT ^ runIdentity

node :: a -> MenuM [Menu a] b -> MenuM [Menu a] ()
node a m = tell $ pure $ Node a $ runMenu m

leaf :: a -> MenuM [Menu a] ()
leaf a = tell $ pure $ Leaf a

-- * Dropdown

dropdown :: forall m. CSS m => Menu Html -> m Html
dropdown tr = do
  parent <- css $ do
    prop "-moz-user-select" "none"
    -- border "1px solid blue"
    width $ prc 100

  let lbs = do
        padding $ rem 0.2 <> rem 0.4
  rule parent lbs
  rule HTML.a lbs

  dropdown <- styled ul $ do
    position "absolute"
    display "none"
    listStyle "none"
    paddingLeft 0
    padding $ rem 0.4 <> 0
    -- border "1px solid red"
    child li $ do
      left 0
  hidden <- css $ display "none"
  rule ("input[type=checkbox]:checked ~ ul" :: TagName) $ display "block"
  menu <- styled ul $ do
    position "relative"
    listStyle "none"
    paddingLeft 0
    display "flex"
    flexFlow "row nowrap"

  let go :: Menu Html -> m Html
      go t = case t of
        Node label' sub -> do
          subHtml <- mapM go sub <&> sequence_
          id <- cssId $ pure ()
          return $ li $ do
            checkbox ! hidden ! id
            label ! parent {- ! for id -} $ label'
            dropdown subHtml
        Leaf html -> return $ li $ html
  go tr <&> menu

-- * Side hovermenu

menuHtml treeC childrenC tree = f tree
  where
    f t = case t of
      Leaf html -> div html
      Node labelText sub -> div ! treeC $ do
        label $ labelText
        case sub of
          [] -> pure ()
          _ -> div ! childrenC $ mapM_ f sub

hoverTree children = css $ do
  position "relative"
  hover $ descendant children $ do
    display "block"

hoverChildren = css $ do
  zIndex 1
  display "none"
  position "absolute"

makeHover rules = do
  children <- hoverChildren
  tree <- hoverTree children
  rule children $ do
    rules
  pure (tree, children)

hoverSide = makeHover $ do
  left $ prc 100
  top 0

hoverBelow = makeHover $ do
  left 0
  top $ prc 100
