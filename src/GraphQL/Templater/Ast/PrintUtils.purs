module GraphQL.Templater.Ast.PrintUtils where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Templater.Positions (Positions)


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