module GraphQL.Templater.LevenshteinDistance
  ( closest
  , distance
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

distance :: String -> String -> Int
distance = distanceImpl

foreign import distanceImpl :: String -> String -> Int

closest :: String -> Array String -> Maybe String
closest = closestImpl >>> map toMaybe

foreign import closestImpl :: String -> Array String -> Nullable String