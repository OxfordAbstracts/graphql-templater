module GraphQL.Templater.Ast.Parser
  ( parse
  , varPartNameParser
  , varPathParser
  , varPathPartParser
  ) where

import Prelude hiding (when)

import Data.Either (Either)
import Data.Foldable (oneOf)
import Data.GraphQL.Parser (listish)
import Data.GraphQL.Parser as GqlParser
import Data.List (List)
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Ast.Argument.Parser (argument, value)
import GraphQL.Templater.Ast.ParseUtils (toString, withPositions)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.Tokens (closeVar, eachClose, eachOpen, openVar, parent, root, withClose, withOpen)
import Parsing (ParseError, Parser, position, runParser)
import Parsing.Combinators (lookAhead, many1Till, manyTill, optionMaybe, sepBy1, try, (<|>))
import Parsing.String (anyChar, char, eof, string)
import Parsing.String.Basic (skipSpaces)

parse
  :: String
  -> Either ParseError (List AstPos)
parse str = runParser str (manyTill astParser eof)

astParser :: Parser String AstPos
astParser = oneOf
  [ eachP
  , withP
  , varP
  , textP
  ]
  where
  varP = withPositions do
    _ <- string openVar
    skipSpaces
    varPath <- varPathParser
    skipSpaces
    _ <- string closeVar
    pure $ Var varPath

  eachP = do
    openStart <- position
    _ <- try $ string eachOpen
    skipSpaces
    varPath <- varPathParser
    skipSpaces
    _ <- string closeVar
    openEnd <- position
    asts <- manyTill astParser (lookAhead $ string eachClose)
    closeStart <- position
    _ <- string eachClose
    closeEnd <- position
    pure $ Each varPath asts
      { start: openStart, end: openEnd }
      { start: closeStart, end: closeEnd }

  withP = do
    openStart <- position
    _ <- try $ string withOpen
    skipSpaces
    varPath <- varPathParser
    skipSpaces
    _ <- string closeVar
    openEnd <- position
    asts <- manyTill astParser (lookAhead $ string withClose)
    closeStart <- position
    _ <- string withClose
    closeEnd <- position
    pure $ With varPath asts
      { start: openStart, end: openEnd }
      { start: closeStart, end: closeEnd }

  textP = withPositions do
    chars <- try $ many1Till anyChar (lookAhead $ void (string openVar) <|> eof)
    pure $ Text $ toString chars

varPathParser :: Parser String (VarPath Positions)
varPathParser = withPositions $ VarPath <$> sepBy1 varPathPartParser (char '.')

varPathPartParser :: Parser String (VarPathPart Positions)
varPathPartParser = withPositions do
  skipSpaces
  name <- varPartNameParser
  skipSpaces
  args <- optionMaybe $ listish "(" ")" (argument value)
  pure $ VarPathPart { name, args }

varPartNameParser :: Parser String (VarPartName Positions)
varPartNameParser = withPositions $
  (VarPartNameParent <$ string parent)
    <|> (VarPartNameRoot <$ string root)
    <|> (VarPartNameGqlName <$> GqlParser.name)

