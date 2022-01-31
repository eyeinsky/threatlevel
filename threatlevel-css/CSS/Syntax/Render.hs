{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module CSS.Syntax.Render (Conf (..)) where

import Common.Prelude hiding ((<+>))
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.DList as D
import Text.Printf
import Data.Text.Format
import Numeric (showHex)

import CSS.Syntax.AST
import Render as R hiding (Conf)
import qualified Render


data Conf = Pretty | Minify

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
    ColorRGB a b c -> pure $ format "rgb({},{},{})" (a,b,c)
    ColorRGBA a b c d -> pure $ format "rgba({},{},{}, {})" (a,b,c,d)
    ColorHSL a b c -> pure $ format "hsl({},{}%,{}%, {})" (a,b,c)
    ColorHSLA a b c d -> pure $ format "hsla({},{}%,{}%, {})" (a,b,c,d)

    Url tl -> pure $ "url(\"" <>  TL.fromStrict tl <> "\")"
    Compound l -> TL.intercalate " " <$> mapM renderM (D.toList l)
    Important -> pure "!important"
    where
      hex a = TL.pack $ showHex a ""
      p = R.tshow

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
    = maybe (pure "") renderM maybeTag
    <+> maybe (pure "") renderM maybeId
    <+> (mconcat <$> mapM renderM cs)
    <+> (mconcat <$> mapM renderM ps)

instance Render Declaration where
  type Conf Declaration = Conf
  renderM (Declaration p v) = R.mseq [pure $ TL.fromStrict p, pure ":", renderM v]
instance Render [Declaration] where
  type Conf [Declaration] = Conf
  renderM ds = TL.concat . map (<> ";") <$> mapM renderM ds

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
    Combined op s s' -> renderM s <+> renderM op <+> renderM s'

instance Render KeyframeSelector where
  type Conf KeyframeSelector = Conf
  renderM ks = pure $ case ks of
    From -> "from"
    To -> "to"
    KPercent d -> TL.pack (printf "%.4f" d) <> "%"

instance Render KeyframeBlock where
  type Conf KeyframeBlock = Conf
  renderM (KeyframeBlock s ds) = renderM s <+> (R.curly <$> renderM ds)

instance Render Rules where
  type Conf Rules = Conf
  renderM li = TL.unlines <$> mapM renderM (filter (not . isEmpty) li)
    where
      isEmpty r = case r of
        Qualified _ ds -> null ds
        Keyframes _ _ -> False
        AtRule _ _ rs -> null rs
        FontFace rs -> null rs

instance Render Rule where
  type Conf Rule = Conf
  renderM r = case r of
    Qualified p ds -> renderM p <+> curlyRules ds
    Keyframes name blocks ->
      pure "@keyframes " <+> pure (TL.fromStrict name) <+> blocks'
      where
        blocks' = R.curly . TL.concat <$> mapM renderM blocks
    AtRule name rule nested -> let
      query = "@" <>  TL.fromStrict name <> " " <>  TL.fromStrict rule
      in pure query <+> (R.curly . TL.unlines <$> mapM renderM nested)
    FontFace ds -> pure "@font-face " <+> curlyRules ds
    where
      curlyRules ds = R.curly <$> (renderM ds)

instance Render Prelude where
  type Conf Prelude = Conf
  renderM (Selectors ss) = TL.intercalate "," <$> mapM renderM ss
