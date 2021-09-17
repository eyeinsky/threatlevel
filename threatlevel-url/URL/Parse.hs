module URL.Parse where

import Prelude
import Data.Coerce
import URL.Core
import Data.Attoparsec.Text as Atto
import qualified Data.Text as TS
import Data.Char
import Data.Maybe (fromJust)

import Control.Applicative


parseURL :: TS.Text -> Either String URL
parseURL = parseToEnd uRLP

parseWebURL :: TS.Text -> Either String WebURL
parseWebURL = parseToEnd webURLP

parseToEnd :: Parser a -> TS.Text -> Either String a
parseToEnd parser input = parseOnly (parser <* endOfInput) input

-- * Parsers (in bottom-up order)

-- ** URL

protoP :: Parser Proto
protoP = Proto . TS.pack <$> (many1 chars <* string "://")
  where
    chars = satisfy isAlphaNum <|> char '+' <|> char '-' <|> char '.'
    -- ^ todo: Use inClass for better speed?

authenticationP :: Parser (Maybe (TS.Text, TS.Text))
authenticationP = some <|> no
  where
    some = do
      user <- Atto.takeWhile (/= ':') <* char ':'
      password <- Atto.takeWhile (/= '@') <* char '@'
      return $ Just (user, password)
    no = pure Nothing

hostP :: Parser Host
hostP = try ip4P <|> domainP

ip4P :: Parser Host
ip4P = IP4 <$> octet' <*> octet' <*> octet' <*> octet
  where
    octet = decimal
    octet' = octet <* char '.'

domainP :: Parser Host
domainP = Domain <$> Atto.takeWhile (/= ':')

portP :: Parser Port
portP = Port <$> (char ':' *> decimal)

authorityP :: Port -> Parser Authority
authorityP defaultPort = do
  authText <- Atto.takeWhile (\c -> not (c == '/' || c == '?' || c == '#'))
  either fail return $ parseOnly authorityP' authText
  where
    authorityP' :: Parser Authority
    authorityP' = Authority <$> authenticationP <*> hostP <*> (portP <|> pure defaultPort)

pathP :: Parser Path
pathP = some <|> no
  where
    some = char '/' *> relativePathP
    no = return (Path [])

relativePathP :: Parser Path
relativePathP = some <|> no
  where
    part = Atto.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#')
    parts = (:) <$> part <*> many (char '/' *> part)
    some = Path <$> parts
    no = return (Path [])

paramsP :: Parser Params
paramsP = some <|> no
  where
    part = Atto.takeWhile (\c -> c /= '#' && c /= '&')
    split = uncurry paramVal . TS.break (== '=') :: TS.Text -> (TS.Text, Maybe TS.Text)
    paramVal a b = if TS.null b
      then (a, Nothing)
      else (a, Just $ TS.tail b)
    some = char '?' >> (Params . map (coerce . split) <$> sepBy part (char '&'))
    no = pure (Params [])

fragmentP :: Parser Fragment
fragmentP = some <|> no
  where
    some = char '#' *> (Fragment <$> takeText)
    no = pure (Fragment "")


uRLP :: Parser URL
uRLP = do
  proto <- protoP
  URL proto
    <$> authorityP (defaultPort proto)
    <*> pathP
    <*> paramsP
    <*> fragmentP

-- * WebURL

webURLP :: Parser WebURL
webURLP =
  try full <|>
  try absolute <|>
  try relative <|>
  fragment
  where
    full = Full <$> uRLP
    absolute = AbsolutePath <$> (char '/' *> pathParamsFragment)
    relative = RelativePath <$> pathParamsFragment
    fragment = FragmentOnly <$> fragmentP
    pathParamsFragment = PathParamsFragment <$> relativePathP <*> paramsP <*> fragmentP

-- * Helpers

defaultPort :: Proto -> Port
defaultPort p = fromJust (lookup p defaultPortMap)

defaultPortMap :: [(Proto, Port)]
defaultPortMap =
  [ ("http", 80)
  , ("https", 443)
  ]
