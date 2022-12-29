module GraphQL.Templater.Lexer
  ( lex
  , toString
  ) where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isSpace)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, oneOf)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits as SCU
import Data.String.Regex.Flags (noFlags)
import GraphQL.Templater.Token (Token(..), TokenWithPos, Tokens)
import Parsing (ParseError, Parser, ParserT, fail, position, runParser)
import Parsing.Combinators (lookAhead, many1Till, manyTill, try, (<|>))
import Parsing.String (anyChar, eof, regex, string)
import Parsing.String.Basic (takeWhile1)

lex :: String -> Either ParseError Tokens
lex str = runParser str (manyTill tokenParser eof)

tokenParser :: Parser String TokenWithPos
tokenParser = addPos $
  oneOf
    [ special
    , WhiteSpace <$> whiteSpace1
    , textP
    ]

  where
  special = oneOf
    [ Else <$ string "{{#else}}"
    , End <$ string "{{#end}}"
    , Null <$ string "null"
    , Equals <$ string "="
    , Colon <$ string ":"
    , Comma <$ string ","
    , OpenBraces <$ string "{{"
    , CloseBraces <$ string "}}"
    , OpenParen <$ string "("
    , CloseParen <$ string ")"
    , OpenSquareBrackets <$ string "["
    , CloseSquareBrackets <$ string "]"
    , Hash <$ string "#"
    , Dot <$ string "."
    , Int <$> int
    , Boolean <$> boolean
    , stringP
    ]

  addPos p = do
    start <- position
    token <- p
    end <- position
    pure $ { token, start, end }

  textP = do
    chars <- try $ many1Till anyChar (lookAhead $ void special <|> eof)
    pure $ Literal $ toString chars

  whiteSpace1 = takeWhile1 isSpace

  stringP = do
    raw <- stringRegex
    pure $ String $ SCU.drop 1 $ SCU.dropRight 1 raw

stringRegex ∷ ∀ m. (ParserT String m String)
stringRegex = case regex "\"(\\\\\"|[^\"])*\"" noFlags of
  Left err -> fail $ "String Regex failed to compile: " <> show err
  Right p -> p

toString :: forall f. Foldable f => f Char -> String
toString = fromCharArray <<< Array.fromFoldable

int :: Parser String Int
int = do
  raw <- regexUnsafe "[0-9]+"
  case Int.fromString raw of
    Just n -> pure n
    Nothing -> fail "Invalid integer"

boolean :: Parser String Boolean
boolean = do 
  b <- regexUnsafe "true|false"
  pure $ b == "true"

regexUnsafe :: String -> Parser String String
regexUnsafe str = case regex str noFlags of
  Left err -> fail $ show str <> " regex failed to compile: " <> show err
  Right p -> p