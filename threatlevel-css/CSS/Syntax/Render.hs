{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module CSS.Syntax.Render (Conf (..)) where

import Common.Prelude hiding ((<+>))
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.DList as D
import Text.Printf
import Numeric (showHex)

import CSS.Syntax.AST
import Render as R hiding (Conf)
import qualified Render


data Conf = Pretty Int | Minify

instance Render Value where
  type Conf Value = Conf
  renderM a = case a of
    Word a -> pure $ TL.fromStrict a
    String ts -> let
      tl = ts
        & TL.fromStrict
        & TL.replace "\\" "\\\\"
        & TL.replace "\"" "\\\""
      in pure $ R.surround "\"" "\"" tl

    Percent a -> pure $ R.tshow a <> "%"
    Em a -> pure $ p a <> "em"
    Rem a -> pure $ p a <> "rem"
    Px a -> pure $ p a <> "px"
    Points a -> pure $ p a <> "pt"
    Int a -> pure $ p a
    Time a -> pure $ p a <> "s"

    ViewportWidth  a -> pure $ p a <> "vw"
    ViewportHeight a -> pure $ p a <> "vh"
    ViewportMin    a -> pure $ p a <> "vmin"
    ViewportMax    a -> pure $ p a <> "vmax"

    ColorHex w32 -> pure $ "#" <> hex w32
    ColorRGB a b c -> pure $ show3 "rgb" a b c
    ColorRGBA a b c d -> pure $ show4 "rgba" a b c d
    ColorHSL a b c -> pure $ show3 "hsl" a b c
    ColorHSLA a b c d -> pure $ show4 "hsla" a b c d

    Url tl -> pure $ "url(\"" <>  TL.fromStrict tl <> "\")"
    Compound l -> TL.intercalate " " <$> mapM renderM (D.toList l)
    Important -> pure "!important"
    where
      hex a = TL.pack $ showHex a ""
      p = R.tshow
      show3 f a b c = f <> par (TL.intercalate "," [p a, p b, p c])
      show4 f a b c d = f <> par (TL.intercalate "," [p a, p b, p c, p d])


instance Render Comment where
  type Conf Comment = Conf
  renderM (Comment a) = pure $ R.sur "/*" "*/"  $ TL.fromStrict a

-- * Selector

instance Render Tag where
  type Conf Tag = Conf
  renderM = \case
    Tag name -> pure $ TL.fromStrict name
    Any -> pure "*"

instance Render Class where
  type Conf Class = Conf
  renderM = pure . TL.fromStrict . ("." <>) . coerce @_ @TS.Text

instance Render Id where
  type Conf Id = Conf
  renderM = pure . TL.fromStrict . ("#" <>) . coerce @_ @TS.Text

instance Render Pseudo where
  type Conf Pseudo = Conf
  renderM p = pure $ case p of
    PseudoClass a arg -> f ":" a arg
    PseudoElement a arg -> f "::" a arg
    where
      f prefix a arg = prefix <> TL.fromStrict a <> maybe "" mkArg arg
      mkArg a = "(" <> TL.fromStrict a <> ")"

instance Render SimpleSelector where
  type Conf SimpleSelector = Conf
  renderM (SimpleSelector maybeTag maybeId cs ps _)
    = do
    text <- maybe (pure "") renderM maybeTag
      <+> maybe (pure "") renderM maybeId
      <+> (mconcat <$> mapM renderM cs)
      <+> (mconcat <$> mapM renderM ps)
    choice id (R.surround "" " ") text

instance Render Declaration where
  type Conf Declaration = Conf
  renderM (Declaration p v) = R.mseq
    [ pure $ TL.fromStrict p
    , pure ":"
    , choice_ "" " "
    , renderM v ]

instance Render [Declaration] where
  type Conf [Declaration] = Conf
  renderM ds = do
    lines <- mapM renderM ds
    indent $ do
      spaces <- getSpaces
      newline <- getNewline
      lines' <- choice (map (<> ";")) (map (R.surround (newline <> spaces) ";")) lines
      return $ TL.concat lines' <> newline

instance Render SOp where
  type Conf SOp = Conf
  renderM s = pure $ case s of
    Descendant -> " "
    Child -> ">"
    Sibling -> "+"
    GeneralSibling -> "~"

instance Render Selector where
  type Conf Selector = Conf
  renderM s = case s of
    Simple ss -> renderM ss
    Combined op s s' ->
      renderM s
      <+> renderM op
      <+> pure " "
      <+> renderM s'

instance Render Rule where
  type Conf Rule = Conf
  renderM (Rule p ds) = do
    spaces <- getSpaces
    newline <- getNewline
    ds' <- renderM ds
    renderM p <+> pure (R.surround "{" (spaces <> "}" <> newline) ds')

instance Render KeyframeSelector where
  type Conf KeyframeSelector = Conf
  renderM ks = pure $ case ks of
    From -> "from"
    To -> "to"
    KPercent d -> TL.pack (printf "%.4f" d) <> "%"

instance Render KeyframeBlock where
  type Conf KeyframeBlock = Conf
  renderM (KeyframeBlock s ds) = renderM s <+> (R.curly <$> renderM ds)

instance Render OuterRules where
  type Conf OuterRules = Conf
  renderM li = mapM renderM li' <&> TL.concat
    where
      li' = filter (not . isEmpty) li
      isEmpty r = case r of
        Plain (Rule _ ds) -> null ds
        Keyframes _ _ -> False
        AtRule _ _ rs -> null rs
        FontFace rs -> null rs

instance Render OuterRule where
  type Conf OuterRule = Conf
  renderM r = do
    spaces <- getSpaces
    choice_ "" spaces <+> case r of
      Plain rule -> renderM rule
      Keyframes name blocks ->
        pure "@keyframes " <+> pure (TL.fromStrict name) <+> blocks'
        where
          blocks' = R.curly . TL.concat <$> mapM renderM blocks
      AtRule name rule nestedRules -> let
        name' = "@" <>  TL.fromStrict name
        rule' = if rule == ""
          then " "
          else R.surround " " " " $ TL.fromStrict rule
        in do
        nestedRules' <- indent $ mapM renderM nestedRules
        spaces <- getSpaces
        newline <- getNewline
        return $
          name'
          <> rule'
          <> R.surround ("{" <> newline) (spaces <> "}") (TL.concat nestedRules')
      FontFace ds -> pure "@font-face " <+> curlyRules ds
    where
      curlyRules ds = R.curly <$> (renderM ds)

instance Render Prelude where
  type Conf Prelude = Conf
  renderM (Selectors ss) = TL.intercalate "," <$> mapM renderM ss

-- * Helpers

choice
  :: (a -> b)
  -> (a -> b)
  -> a -> Reader Conf b
choice minify pretty m = ask >>= \case
  Minify -> return $ minify m
  Pretty _ -> return $ pretty m

choice_ :: a -> a -> Reader Conf a
choice_ minify pretty = ask >>= \case
  Minify -> pure minify
  Pretty _ -> pure pretty

indent :: Reader Conf a -> Reader Conf a
indent m = local (inc 4) m

inc :: Int -> Conf -> Conf
inc m = \case
  Minify -> Minify
  Pretty n -> Pretty $ n + m

getSpaces :: Reader Conf Render.Text
getSpaces = ask >>= \case
  Minify -> pure ""
  Pretty n -> pure $ TL.replicate (fromIntegral n) " "

getNewline :: Reader Conf Render.Text
getNewline = ask >>= \case
  Minify -> pure ""
  Pretty{} -> pure "\n"
