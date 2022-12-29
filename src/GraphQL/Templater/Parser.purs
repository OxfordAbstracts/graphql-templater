module GraphQL.Templater.Parser (parse) where

import Prelude hiding (when)

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (class Foldable, oneOf)
import Data.GraphQL.Parser (arguments)
import Data.GraphQL.Parser as GqlAst
import Data.List (List)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Positions (Positions)
import Parsing (ParseError, Parser, ParserT, position, runParser)
import Parsing.Combinators (lookAhead, many1Till, manyTill, optionMaybe, sepBy1, try, (<|>))
import Parsing.String (anyChar, char, eof, string)
import Parsing.String.Basic (skipSpaces)

parse
  :: String
  -> Either ParseError (List AstPos)
parse str = runParser str (manyTill astParser eof)

astParser :: Parser String AstPos
astParser = withPositions $ oneOf
  [ eachP
  , varP
  , Text <$> textP
  ]
  where
  varP = do
    _ <- string "{{"
    skipSpaces
    varPath <- varPathParser
    skipSpaces
    _ <- string "}}"
    pure $ Var varPath

  eachP = do
    _ <- try $ string "{{#each"
    skipSpaces
    varPath <- varPathParser
    skipSpaces
    _ <- string "}}"
    asts <- manyTill astParser (string "{{/each}}")
    pure $ Each varPath asts

  textP = do
    chars <- try $ many1Till anyChar (lookAhead $ void (string "{{") <|> eof)
    pure $ toString chars

varPathParser :: Parser String (VarPath Positions)
varPathParser = withPositions $ VarPath <$> sepBy1 varPathPartParser (char '.')

varPathPartParser :: Parser String (VarPathPart Positions)
varPathPartParser = withPositions do
  skipSpaces
  name <- varPartNameParser
  skipSpaces
  args <- optionMaybe $ withPositions $ Tuple <$> arguments
  pure $ VarPathPart { name, args }

varPartNameParser :: Parser String (VarPartName Positions)
varPartNameParser = withPositions $
  (VarPartNameParent <$ string "$parent")
    <|> (VarPartNameRoot <$ string "$root")
    <|> (VarPartNameGqlName <$> GqlAst.name)

withPositions
  :: forall b m a
   . ParserT m a (Positions -> b)
  -> ParserT m a b
withPositions p = do
  start <- position
  x <- p
  end <- position
  pure $ x { start, end }

toString :: forall f. Foldable f => f Char -> String
toString = fromCharArray <<< Array.fromFoldable
