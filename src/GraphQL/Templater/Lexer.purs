module GraphQL.Templater.Lexer
  ( lex
  , toString
  )
  where

import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isSpace)
import Data.Either (Either)
import Data.Foldable (class Foldable, oneOf)
import Data.List (List, (:))
import Data.String.CodeUnits (fromCharArray)
import GraphQL.Templater.Token (Token(..), TokenWithPos, Tokens)
import Parsing (ParseError, Parser, ParserT, Position, position, runParser)
import Parsing.Combinators (lookAhead, many, many1Till, manyTill, optionMaybe, try, (<|>))
import Parsing.String (anyChar, char, eof, regex, string)
import Parsing.String.Basic (alphaNum, letter, skipSpaces, takeWhile1, whiteSpace)

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
    , Equals <$ string "="
    , Colon <$ string ":"
    , Comma <$ string ","
    , OpenBraces <$ string "{{"
    , CloseBraces <$ string "}}"
    , OpenParen <$ string "("
    , CloseParen <$ string ")"
    , Hash <$ string "#"
    , Dot <$ string "."
    ]

  addPos p = do
    start <- position
    token <- p
    end <- position
    pure $ { token, start, end }

  textP = do
    chars <- try $ many1Till anyChar (lookAhead $ void special <|> eof)
    pure $ Text $ toString chars

  whiteSpace1 = takeWhile1 isSpace

--   close = void $ string "}}"

-- withPositions
--   :: forall a s m
--    . ParserT s m
--        ( { end :: Position
--          , start :: Position
--          }
--          -> a
--        )
--   -> ParserT s m a
-- withPositions p = do
--   start <- position
--   x <- p
--   end <- position
--   pure $ x { start, end }

-- identifier :: Parser String String
-- identifier = do
--   first <- letter
--   rest <- many (alphaNum <|> char '_' <|> char '.' <|> char '/')
--   pure $ toString $ first : rest

toString :: forall f. Foldable f => f Char -> String
toString = fromCharArray <<< Array.fromFoldable