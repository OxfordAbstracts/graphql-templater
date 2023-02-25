module GraphQL.Templater.Ast.PrintUtils where

import Prelude

import Data.List (List(..), fold, (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (class Foldable, class Traversable)
import GraphQL.Templater.Positions (Positions)
import Parsing (Position(..))

-- displayPrintResult :: PrintResult Int -> String
-- displayPrintResult result =
--   evalState result 0
--     # (Map.toUnfoldable :: _ -> List _)
--     <#> snd
--     # fold

-- displayPositionedPrintResult :: PrintResult Position -> String
-- displayPositionedPrintResult result =
--   evalState result 0
--     # (Map.toUnfoldable :: _ -> List _)
--     # foldl insert ""
--   where
--   insert :: String -> Tuple Position String -> String
--   insert res (Tuple (Position { index }) str) =
--     if String.length res < index then
--       res <> unsafeRepeat (index - String.length res) " " <> str
--     else
--       case String.splitAt index res of
--         { before, after } ->
--           before <> str <> String.drop (String.length str) after

-- type String = State Int (Map k String)

adjustPosition :: Int -> Position -> Position
adjustPosition n (Position { index, column, line }) =
  Position
    { index: index + n
    , column: column + n
    , line
    }

combine :: forall f. Foldable f => Traversable f => f (String) -> (String)
combine results = fold results

empty ::  String
empty = ""

-- atStart :: Int -> String -> String
atStart :: forall t31 t32. t31 -> t32 -> t32
atStart pos str = str

-- atEnd :: Int -> String -> String
atEnd :: forall t34 t3237. t34 -> t3237 -> t3237
atEnd pos str = atStart unit str

mapWithPrevious :: forall a b. (Maybe a -> a -> b) -> List a -> List b
mapWithPrevious f = go Nothing
  where
  go prev xs = case xs of
    Nil -> Nil
    Cons x xs' -> f prev x : go (Just x) xs'

dummyPositions :: forall f a. Functor f => f a -> f Positions
dummyPositions = map $ const initalPositions

initalPositions :: Positions
initalPositions = { start: 0, end: 0, str: "" }