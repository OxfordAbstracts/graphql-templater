module GraphQL.Templater.Ast.ParseUtils where

import Prelude hiding (when)

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.String.CodeUnits (fromCharArray)
import GraphQL.Templater.Positions (Positions)
import Parsing (ParserT, position)

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