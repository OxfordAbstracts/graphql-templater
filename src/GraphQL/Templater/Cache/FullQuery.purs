module GraphQL.Templater.Cache.FullQuery where

import Data.Argonaut.Core (Json)
import Data.Map as Map

type FullQueryCache = Map.Map String Json

