module GraphQL.Templater.Ast.Print
  ( printTemplateAsts
  ) where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.GraphQL.AST.Print (printAst)
import Data.List (List(..), foldl, last, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Utils (unsafeRepeat)
import Data.Tuple (Tuple(..))
import GraphQL.Templater.Ast (Arg(..), ArgName(..), Ast(..), Value(..), VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Positions (Positions)
import Parsing (Position(..))

printTemplateAsts :: List (Ast Positions) -> String
printTemplateAsts = displayPrintResult <<< printMapTemplateAsts

displayPrintResult :: PrintResult -> String
displayPrintResult =
  (Map.toUnfoldable :: _ -> List _)
    >>> foldl insert ""
  where
  insert :: String -> Tuple Position String -> String
  insert res (Tuple (Position { index }) str) =
    if String.length res < index then
      res <> unsafeRepeat (index - String.length res) " " <> str
    else
      case String.splitAt index res of
        { before, after } ->
          before <> str <> after

type PrintResult = Map Position String

printMapTemplateAsts :: List (Ast Positions) -> PrintResult
printMapTemplateAsts asts = Map.unions $ map printMapTemplateAst asts

printMapTemplateAst :: Ast Positions -> PrintResult
printMapTemplateAst = case _ of
  Var varPath { start, end } ->
    Map.unions
      [ atStart start "{{"
      , printMapVarPath varPath
      , atEnd end "}}"
      ]
  Each varPath@(VarPath _ varPathPos) inner { start, end } ->
    Map.unions
      [ atStart start "{{#each "
      , printMapVarPath varPath
      , atStart varPathPos.end "}}"
      , printMapTemplateAsts inner
      , atEnd end "{{/each}}"
      ]
  With varPath@(VarPath _ varPathPos) inner { start, end } ->
    Map.unions
      [ atStart start "{{#with "
      , printMapVarPath varPath
      , atStart varPathPos.end "}}"
      , printMapTemplateAsts inner
      , atEnd end "{{/with}}"
      ]

  Text text { start } ->
    atStart start text

printMapVarPath :: VarPath Positions -> PrintResult
printMapVarPath (VarPath path _) = Map.unions $ mapWithIndex printMapVarPathPart path

printMapVarPathPart :: Int -> VarPathPart Positions -> PrintResult
printMapVarPathPart idx (VarPathPart { name, args } { start }) =
  Map.unions
    [ dot
    , printMapVarPartName name
    , printMapArgs args
    ]
  where
  dot =
    if idx == 0 then
      Map.empty
    else
      Map.singleton (adjustPosition (-1) start) "."

printMapVarPartName :: VarPartName Positions -> PrintResult
printMapVarPartName = case _ of
  VarPartNameGqlName gqlName { start } ->
    atStart start gqlName
  VarPartNameParent { start } ->
    atStart start "*parent"
  VarPartNameRoot { start } ->
    atStart start "*root"

printMapArgs :: Maybe (List (Arg Positions)) -> PrintResult
printMapArgs = case _ of
  Nothing -> Map.empty
  Just Nil -> Map.empty
  Just list@((Arg _ { start, end }) : _) ->
    let
      argsEnd = adjustPosition 1 case last list of
        Just (Arg _ pos) -> pos.end
        _ -> end
    in
      Map.unions $
        pure (atEnd start "(")
          <> pure (atEnd argsEnd ")")
          <> mapWithIndex printMapArg list

printMapArg :: Int -> Arg Positions -> PrintResult
printMapArg idx (Arg { name, value: Value value valuePos } { start }) =
  Map.unions
    [ comma
    , printMapArgName name
    , atStart valuePos.start (printAst value)
    ]
  where
  comma =
    if idx == 0 then
      Map.empty
    else
      atEnd start ", "

printMapArgName :: ArgName Positions -> PrintResult
printMapArgName (ArgName name { start, end }) = Map.unions
  [ atStart start name
  , atStart (adjustPosition 0 end) ": "
  ]

atStart :: Position -> String -> PrintResult
atStart = Map.singleton

atEnd :: Position -> String -> PrintResult
atEnd pos str = Map.singleton (adjustPosition (-(String.length str)) pos) str

adjustPosition :: Int -> Position -> Position
adjustPosition n (Position { index, column, line }) =
  Position
    { index: index + n
    , column: column + n
    , line
    }
