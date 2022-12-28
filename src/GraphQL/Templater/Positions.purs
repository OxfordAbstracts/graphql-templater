module GraphQL.Templater.Positions where

import Parsing (Position)

type Positions = { start :: Position, end :: Position }
