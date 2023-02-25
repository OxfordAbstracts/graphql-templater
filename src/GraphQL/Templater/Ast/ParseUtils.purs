module GraphQL.Templater.Ast.ParseUtils where

import Prelude hiding (when)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.String (splitAt)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import GraphQL.Templater.Positions (Positions)
import Parsing (ParserT, Position(..), position)
import Parsing.Combinators (lookAhead)
import Parsing.String (consumeWith)

withPositions
  :: forall a m
   . ParserT String m (Positions -> a)
  -> ParserT String m a
withPositions p = andPositions p <#> \(Tuple pos x) -> x pos

andPositions :: forall m a. ParserT String m a -> ParserT String m (Tuple Positions a)
andPositions p =
  do
    { start: Position start, end: Position end, x } <- lookAhead do
      start <- position
      x <- p
      end <- position
      pure { start, end, x }
    str <- consumeWith \str' -> Right
      let
        { before, after } = splitAt (end.index - start.index) str'
      in
        { consumed: before
        , remainder: after
        , value: before
        }

    pure $ Tuple { start: start.index, end: end.index, str } x

toString :: forall f. Foldable f => f Char -> String
toString = fromCharArray <<< Array.fromFoldable