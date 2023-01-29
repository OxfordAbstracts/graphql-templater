module GraphQL.Templater.Ast.Print
  ( printPositioned
  , printUnpositioned
  ) where

import Prelude

import Control.Monad.State (State, evalState, modify)
import Data.Bifunctor (lmap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.GraphQL.AST.Print (printAst)
import Data.List (List(..), fold, foldl, last, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Utils (unsafeRepeat)
import Data.Traversable (class Foldable, class Traversable, sequence)
import Data.Tuple (Tuple(..), snd)
import Debug (spy, spyWith)
import Foreign.Object as Object
import GraphQL.Templater.Ast (Arg(..), ArgName(..), Ast(..), Value(..), VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Positions (Positions)
import Parsing (Position(..))

-- | Print an AST, keeping the original positions of the tokens.
-- | The asts must have the correct positions set.
printPositioned :: List (Ast Positions) -> String
printPositioned = displayPositionedPrintResult <<< printMapTemplateAsts

displayPositionedPrintResult :: PrintResult Position -> String
displayPositionedPrintResult result =
  evalState result 0
    # spyWith "map" ((Map.toUnfoldable :: _ -> List _ ) >>> map (lmap show) >>> Object.fromFoldable)
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
          before <> str <> after

-- | Print an AST, discarding the original positions of the tokens.
printUnpositioned :: List (Ast Positions) -> String
printUnpositioned = displayPrintResult <<< printMapTemplateAsts

displayPrintResult :: PrintResult Int -> String
displayPrintResult result =
  evalState result 0
    # (Map.toUnfoldable :: _ -> List _)
    <#> snd
    # fold

type PrintResult k = State Int (Map k String)

printMapTemplateAsts :: forall k. PrintKey k => List (Ast Positions) -> PrintResult k
printMapTemplateAsts asts = combine $ map printMapTemplateAst asts

printMapTemplateAst :: forall k. PrintKey k => Ast Positions -> PrintResult k
printMapTemplateAst = case _ of
  Var varPath { start, end } ->
    combine
      [ atStart start "{{"
      , printMapVarPath varPath
      , atEnd end "}}"
      ]
  Each varPath@(VarPath _ varPathPos) inner open close ->
    combine
      [ atStart open.start "{{#each "
      , printMapVarPath varPath
      , atStart varPathPos.end "}}"
      , printMapTemplateAsts inner
      , atStart close.start "{{/each}}"
      ]
  With varPath@(VarPath _ varPathPos) inner open close ->
    combine
      [ atStart open.start "{{#with "
      , printMapVarPath varPath
      , atStart varPathPos.end "}}"
      , printMapTemplateAsts inner
      , atStart close.start "{{/with}}"
      ]

  Text text { start } ->
    atStart start text

printMapVarPath :: forall k. PrintKey k => VarPath Positions -> PrintResult k
printMapVarPath (VarPath path _) = combine $ mapWithIndex printMapVarPathPart path

printMapVarPathPart :: forall k. PrintKey k => Int -> VarPathPart Positions -> PrintResult k
printMapVarPathPart idx (VarPathPart { name, args } { start }) =
  combine
    [ dot
    , printMapVarPartName name
    , printMapArgs args
    ]
  where
  dot =
    if idx == 0 then
      empty
    else
      atEnd start "."

printMapVarPartName :: forall k. PrintKey k => VarPartName Positions -> PrintResult k
printMapVarPartName = case _ of
  VarPartNameGqlName gqlName { start } ->
    atStart start gqlName
  VarPartNameParent { start } ->
    atStart start "*parent"
  VarPartNameRoot { start } ->
    atStart start "*root"

printMapArgs :: forall k. PrintKey k => Maybe (List (Arg Positions)) -> PrintResult k
printMapArgs = case _ of
  Nothing -> empty
  Just Nil -> empty
  Just list@((Arg _ { start, end }) : _) ->
    let
      argsEnd = adjustPosition 1 case last list of
        Just (Arg _ pos) -> pos.end
        _ -> end
    in
      combine $
        pure (atEnd start "(")
          <> mapWithIndex printMapArg list
          <> pure (atEnd argsEnd ")")

printMapArg :: forall k. PrintKey k => Int -> Arg Positions -> PrintResult k
printMapArg idx (Arg { name, value: Value value valuePos } { start }) =
  combine
    [ comma
    , printMapArgName name
    , atStart valuePos.start (printAst value)
    ]
  where
  comma =
    if idx == 0 then
      empty
    else
      atEnd start ", "

printMapArgName :: forall k. PrintKey k => ArgName Positions -> PrintResult k
printMapArgName (ArgName name { start, end }) = combine
  [ atStart start name
  , atStart end ": "
  ]

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

adjustPosition :: Int -> Position -> Position
adjustPosition n (Position { index, column, line }) =
  Position
    { index: index + n
    , column: column + n
    , line
    }

class Ord k <= PrintKey k where
  getKey :: Position -> State Int k

instance PrintKey Position where
  getKey pos = pure pos

instance PrintKey Int where
  getKey _ = modify ((+) 1)