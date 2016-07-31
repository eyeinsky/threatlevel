module Web.FRP where

import Prelude hiding (length)
import JS

point = do
  id <- new $ ulit (-1)
  getId <- newf $ do
    id .+= ulit 1
    retrn id
  newf $ \fn -> do
    id <- new $ call0 getId
    observers <- new $ ulit ([] :: [Expr ()])
    add <- newf' "add" $ \listener -> do
      consoleLog [listener !. "id", ulit "added"]
      bare $ push listener observers
    remove <- newf' "remove" $ \listener -> do
      i <- new $ indexOf observers listener
      ifelse (i .>= ulit 0) (do
        removed <- bare $ splice observers i (ulit 1)
        consoleLog [listener !. "id", ulit "removed"]
        retrn true
        ) (do
        consoleLog [listener !. "id", ulit "not found"]
        retrn false
        )
    once <- newf' "once" $ \listener -> do
      send0 <- new $ listener !. "send"
      wrap <- newf $ \ev -> do
        bare $ call1 send0 ev
        retrn $ call1 remove listener
      listener !. "send" .= wrap
      bare $ call1 add listener
    send0 <- newf' "send0" $ \ev -> do
      len <- new $ length observers
      i <- new $ ulit 0
      for (i .< len) $ do
        bare $ call1 ((observers .! i) !.  "send") ev
        i .+= ulit 1
    let ter = ternary (call1 (ex "typeof") fn .=== ulit "function")
    send <- new' "send" =<< (ter <$> (do
      func $ \ev -> do
        bare $ call1 fn ev
        consoleLog [id, ulit "executed"]
        bare $ call1 send0 ev
      ) <*> (func $ \ev -> bare $ call1 send0 ev))

    retrn $ ulit
      [ ("add", Cast add)
      , ("once", Cast once)
      , ("remove", Cast remove)
      , ("send", Cast send)
      , ("observers", Cast observers)
      , ("id", Cast id)
      ]

{-
var p1 = point(function() {
  console.log('jee');
})

var p2 = point(function () {
  var d = new Date()
  document.getElementById('o1').innerHTML = d.toString()
})

var p3 = point(function () {
  var d = new Date()
  document.getElementById('o2').innerHTML = d.toString()
})

p1.add(p2)
p1.once(p3)
-}
