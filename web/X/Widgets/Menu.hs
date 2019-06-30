module X.Widgets.Menu where

import Control.Monad.Writer
import X.Prelude
import X
import qualified HTML

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

dropdown :: forall m. MonadWeb m => Menu Html -> m Html
dropdown tr = do
  parent <- css $ do
    prop "-moz-user-select" "none"
    -- border "1px solid blue"
    width $ prc 100

  let lbs = do
        padding $ rem 0.2 <> rem 0.4
  cssRule parent lbs
  cssRule HTML.a lbs

  dropdown <- styleds ul $ do
    position "absolute"
    display "none"
    listStyle "none"
    paddingLeft 0
    padding $ rem 0.4 <> 0
    -- border "1px solid red"
    child li $ do
      left 0
  hidden <- css $ display "none"
  cssRule ("input[type=checkbox]:checked ~ ul" :: TagName) $ display "block"
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
          id <- newId
          return $ li $ do
            checkbox ! hidden ! id
            label ! parent ! X.for id $ label'
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
  cssRule children $ do
    rules
  pure (tree, children)

hoverSide = makeHover $ do
  left $ prc 100
  top 0

hoverBelow = makeHover $ do
  left 0
  top $ prc 100
