module GraphQL.Templater.Lexer where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Parsing (ParseError, Parser, ParserT, Position, position, runParser)
import Parsing.Combinators (lookAhead, many, many1Till, manyTill, try, (<|>))
import Parsing.String (anyChar, char, eof, string)
import Parsing.String.Basic (alphaNum, letter, skipSpaces)

data Token a
  = Each String a
  | If String a
  | Else a
  | End a
  | Var String a
  | Text String a
  | EOF

derive instance Generic (Token a) _

derive instance Functor Token

derive instance eqToken :: Eq a => Eq (Token a)

instance Show a => Show (Token a) where
  show = genericShow

type Positions = { start :: Position, end :: Position }

lex :: String -> Either ParseError (List (Token Positions))
lex str = runParser str (manyTill tokenParser eof)

tokenParser :: Parser String (Token Positions)
tokenParser = 
  if_
  <|> each_
  <|> end_
  <|> else_
  <|> var_
  <|> text_
  where
  if_ = withPositions do
    try $ void $ string "{{#if"
    skipSpaces
    name <- identifier
    skipSpaces
    close
    pure $ If name

  each_ = withPositions do
    try $ void $ string "{{#each"
    skipSpaces
    name <- identifier
    skipSpaces
    close
    pure $ Each name

  end_ = withPositions do
    try $ void $ string "{{end}}"
    pure End

  else_ = withPositions do
    try $ void $ string "{{else}}"
    pure Else

  var_ = withPositions do
    try $ void $ string "{{"
    skipSpaces
    name <- identifier
    skipSpaces
    close
    pure $ Var name

  text_ = withPositions do
    chars <- try $ many1Till anyChar (lookAhead $ void (string "{{") <|> eof)
    pure $ Text $ toString chars

  close = void $ string "}}"

withPositions
  :: forall a s m
   . ParserT s m
       ( { end :: Position
         , start :: Position
         }
         -> a
       )
  -> ParserT s m a
withPositions p = do
  start <- position
  x <- p
  end <- position
  pure $ x { start, end }

identifier :: Parser String String
identifier = do
  first <- letter
  rest <- many (alphaNum <|> char '_' <|> char '.')
  pure $ toString $ first : rest

toString :: forall f. Foldable f => f Char -> String
toString = fromCharArray <<< Array.fromFoldable