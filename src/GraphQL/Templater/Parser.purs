module GraphQL.Templater.Parser
  ( parse
  , varPartNameParser
  ) where

import Prelude hiding (when)

import Data.Array as Array
import Data.Either (Either)
import Data.Foldable (class Foldable, oneOf)
import Data.GraphQL.AST as GqlAst
import Data.GraphQL.Parser (listish)
import Data.GraphQL.Parser as GqlParser
import Data.List (List)
import Data.String.CodeUnits (fromCharArray)
import GraphQL.Templater.Ast (Arg(..), ArgName(..), Ast(..), AstPos, Value(..), VarPartName(..), VarPath(..), VarPathPart(..))
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
  , textP
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
    pure $ Text $ toString chars

varPathParser :: Parser String (VarPath Positions)
varPathParser = withPositions $ VarPath <$> sepBy1 varPathPartParser (char '.')

varPathPartParser :: Parser String (VarPathPart Positions)
varPathPartParser = withPositions do
  skipSpaces
  name <- varPartNameParser
  skipSpaces
  args <- optionMaybe $ listish "(" ")" (argument GqlParser.value)
  pure $ VarPathPart { name, args }

argument ∷ Parser String (GqlAst.Value) → Parser String (Arg Positions)
argument vc = withPositions
  $ map Arg
  $ { name: _, value: _ }
      <$> (withPositions $ ArgName <$> GqlParser.name)
      <*> (withPositions $ map Value $ GqlParser.ignoreMe *> char ':' *> GqlParser.ignoreMe *> vc)

varPartNameParser :: Parser String (VarPartName Positions)
varPartNameParser = withPositions $
  (VarPartNameParent <$ string "*parent")
    <|> (VarPartNameRoot <$ string "*root")
    <|> (VarPartNameGqlName <$> GqlParser.name)

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
