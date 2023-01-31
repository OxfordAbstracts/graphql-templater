module GraphQL.Templater.Ast.Print
  ( printPositioned
  , printUnpositioned
  ) where

import Prelude

import Control.Monad.State (evalState)
import Data.List (List(..), fold, last, (:))
import Data.List.Types (toList)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (snd)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..), Args)
import GraphQL.Templater.Ast.Argument (ArgName(..), Argument(..))
import GraphQL.Templater.Ast.Argument.Print (printValue)
import GraphQL.Templater.Ast.PrintUtils (class PrintKey, PrintResult, adjustPosition, atEnd, atStart, combine, displayPositionedPrintResult, empty, mapWithPrevious)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.Tokens (closeVar, eachClose, eachOpen, openVar, parent, root, withClose, withOpen)

-- | Print an AST, keeping the original positions of the tokens.
-- | The asts must have the correct positions set.
printPositioned :: List (Ast Positions) -> String
printPositioned = displayPositionedPrintResult <<< printMapTemplateAsts

-- | Print an AST, discarding the original positions of the tokens.
printUnpositioned :: List (Ast Positions) -> String
printUnpositioned = displayPrintResult <<< printMapTemplateAsts

displayPrintResult :: PrintResult Int -> String
displayPrintResult result =
  evalState result 0
    # (Map.toUnfoldable :: _ -> List _)
    <#> snd
    # fold

printMapTemplateAsts :: forall k. PrintKey k => List (Ast Positions) -> PrintResult k
printMapTemplateAsts asts = combine $ map printMapTemplateAst asts

printMapTemplateAst :: forall k. PrintKey k => Ast Positions -> PrintResult k
printMapTemplateAst = case _ of
  Var varPath { start, end } ->
    combine
      [ atStart start openVar
      , printMapVarPath varPath
      , atEnd end closeVar
      ]
  Each varPath@(VarPath _ varPathPos) inner open close ->
    combine
      [ atStart open.start $ eachOpen
      , printMapVarPath varPath
      , atStart varPathPos.end closeVar
      , printMapTemplateAsts inner
      , atStart close.start eachClose
      ]
  With varPath@(VarPath _ varPathPos) inner open close ->
    combine
      [ atStart open.start withOpen
      , printMapVarPath varPath
      , atStart varPathPos.end closeVar
      , printMapTemplateAsts inner
      , atStart close.start withClose
      ]

  Text text { start } ->
    atStart start text

printMapVarPath :: forall k. PrintKey k => VarPath Positions -> PrintResult k
printMapVarPath (VarPath path _) = combine $ mapWithPrevious printMapVarPathPart (toList path)

printMapVarPathPart :: forall k. PrintKey k => Maybe (VarPathPart Positions) -> VarPathPart Positions -> PrintResult k
printMapVarPathPart prev (VarPathPart { name, args } {}) =
  combine
    [ dot
    , printMapVarPartName name
    , printMapArgs args
    ]
  where
  dot = case prev of
    Nothing -> empty
    Just (VarPathPart _ { end }) ->
      atStart end "."

printMapVarPartName :: forall k. PrintKey k => VarPartName Positions -> PrintResult k
printMapVarPartName = case _ of
  VarPartNameGqlName gqlName { start } ->
    atStart start gqlName
  VarPartNameParent { start } ->
    atStart start parent
  VarPartNameRoot { start } ->
    atStart start root

printMapArgs :: forall k. PrintKey k => Maybe (Args Positions) -> PrintResult k
printMapArgs = case _ of
  Nothing -> empty
  Just Nil -> empty
  Just args@((Argument { pos: { start, end } }) : _) ->
    let
      argsEnd = adjustPosition 1 case last args of
        Just (Argument { pos }) -> pos.end
        _ -> end
    in
      combine $
        pure (atEnd start "(")
          <> mapWithPrevious printMapArg args
          <> pure (atEnd argsEnd ")")

printMapArg :: forall k. PrintKey k => Maybe (Argument Positions) -> Argument Positions -> PrintResult k
printMapArg prev (Argument { name, value }) =
  combine
    [ comma
    , printMapArgName name
    , printValue value
    ]
  where
  comma =
    case prev of
      Nothing -> empty
      Just (Argument { pos: { end } }) -> atStart end ","

printMapArgName :: forall k. PrintKey k => ArgName Positions -> PrintResult k
printMapArgName (ArgName name { start, end }) = combine
  [ atStart start name
  , atStart end ": "
  ]

