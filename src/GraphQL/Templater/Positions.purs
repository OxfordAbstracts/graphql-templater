module GraphQL.Templater.Positions where

import Parsing (Position)

type Positions =
  { start :: Int
  , end :: Int
  , str :: String
  }

