module GraphQL.Templater.Ast.Suggest where

import Prelude

import Data.List (List(..))
import GraphQL.Templater.Ast (Ast)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeDefs (GqlTypeTree)

suggestEaches :: GqlTypeTree -> List (Ast Positions) -> Int -> List String
suggestEaches typeTree ast idx = Nil