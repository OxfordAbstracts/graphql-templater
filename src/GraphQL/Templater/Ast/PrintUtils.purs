module GraphQL.Templater.Ast.PrintUtils where

import Prelude

import Control.Monad.State (State, evalState, modify)
import Data.List (List(..), fold, foldl, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Utils (unsafeRepeat)
import Data.Traversable (class Foldable, class Traversable, sequence)
import Data.Tuple (Tuple(..), snd)
import Parsing (Position(..))

displayPrintResult :: PrintResult Int -> String
displayPrintResult result =
  evalState result 0
    # (Map.toUnfoldable :: _ -> List _)
    <#> snd
    # fold

displayPositionedPrintResult :: PrintResult Position -> String
displayPositionedPrintResult result =
  evalState result 0
    # (Map.toUnfoldable :: _ -> List _)
    # foldl insert ""
  where
  insert :: String -> Tuple Position String -> String
  insert res (Tuple (Position { index }) str) =
    if String.length res < index then
      res <> unsafeRepeat (index - String.length res) " " <> str
    else
      case String.splitAt index res of
        { before, after } ->
          before <> str <> String.drop (String.length str) after

type PrintResult k = State Int (Map k String)

class Ord k <= PrintKey k where
  getKey :: Position -> State Int k

instance PrintKey Position where
  getKey pos = pure pos

instance PrintKey Int where
  getKey _ = modify ((+) 1)

adjustPosition :: Int -> Position -> Position
adjustPosition n (Position { index, column, line }) =
  Position
    { index: index + n
    , column: column + n
    , line
    }

combine :: forall f k. Ord k => Foldable f => Traversable f => f (PrintResult k) -> (PrintResult k)
combine results = do
  f <- sequence results
  pure $ Map.unions f

empty :: forall k. PrintResult k
empty = pure Map.empty

atStart :: forall k. PrintKey k => Position -> String -> PrintResult k
atStart pos str = do
  k <- getKey pos
  pure $ Map.singleton k str

atEnd :: forall k. PrintKey k => Position -> String -> PrintResult k
atEnd pos str = atStart (adjustPosition (-(String.length str)) pos) str

mapWithPrevious :: forall a b. (Maybe a -> a -> b) -> List a -> List b
mapWithPrevious f = go Nothing
  where
  go prev xs = case xs of
    Nil -> Nil
    Cons x xs' -> f prev x : go (Just x) xs'