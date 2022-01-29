
{- ** ANOTHER DREAM :) ** -}

anotherDream = do

  let log a = bare $ call1 (ex "console" !. "log") a

  x <- const 5
  y <- const 6
  z <- const 7

  f <- func $ \a b c -> do

    c <- class_ $ do
      backgroundColor "greend"
      color "yellow"

--    createHtmls $ div ! c $ text "my box"

    log a
    log b
    log c

  bare $ call f [x, y, z]

hot = printJS anotherDream
