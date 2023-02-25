module GraphQL.Templater.Ast.Print
  ( printEach
  , printMapVarPath
  , printMapVarPathPart
  , printPositioned
  , printSingleAstPositioned
  , printUnpositioned
  , printVarPartName
  , printWith
  )
  where

import Prelude

import Data.List (List(..), foldMap, (:))
import Data.List.Types (toList)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (guard)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..), Args)
import GraphQL.Templater.Ast.Argument (ArgName(..), Argument(..))
import GraphQL.Templater.Ast.Argument.Print (printValue)
import GraphQL.Templater.Ast.PrintUtils (atEnd, atStart, combine, empty, mapWithPrevious)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.Tokens (closeVar, eachClose, eachOpen, openVar, parent, root, withClose, withOpen)

-- | Print an AST, keeping the original positions of the tokens.
printPositioned :: List (Ast Positions) -> String
printPositioned asts = foldMap printSingleAstPositioned asts

printSingleAstPositioned :: Ast Positions -> String
printSingleAstPositioned = case _ of
  Text _ { str } -> str
  Var _ { str } -> str
  Each _ inner open close -> open.str <> printPositioned inner <> close.str
  With _ inner open close -> open.str <> printPositioned inner <> close.str

-- -- | Print an AST, discarding the original positions of the tokens.
printUnpositioned :: forall a. List (Ast a) -> String
printUnpositioned asts = combine $ map printMapTemplateAst asts

-- -- asUnpositioned fn = displayPrintResult <<< fn <<< map dummyPositions

printMapTemplateAsts :: forall a. List (Ast a) -> String
printMapTemplateAsts asts = combine $ map printMapTemplateAst asts

printMapTemplateAst :: forall a. Ast a -> String
printMapTemplateAst = case _ of
  Var varPath _ ->
    combine
      [ atStart unit openVar
      , printMapVarPath varPath
      , atEnd unit closeVar
      ]
  Each varPath inner open close ->
    printEach varPath inner open close

  With varPath inner open close ->
    printWith varPath inner open close

  Text text _ ->
    atStart unit text

printEach :: forall a. VarPath a -> List (Ast a) -> a -> a -> String
printEach varPath@(VarPath _ _) inner open close =
  combine
    [ atStart unit $ eachOpen
    , printMapVarPath varPath
    , atStart unit closeVar
    , printMapTemplateAsts inner
    , atStart unit eachClose
    ]

printWith :: forall a. VarPath a -> List (Ast a) -> a -> a -> String
printWith varPath@(VarPath _ varPathPos) inner open close =
  combine
    [ atStart unit withOpen
    , printMapVarPath varPath
    , atStart unit closeVar
    , printMapTemplateAsts inner
    , atStart unit withClose
    ]

printMapVarPath :: forall a. VarPath a -> String
printMapVarPath (VarPath path _) = combine $ mapWithPrevious printMapVarPathPart (toList path)

printMapVarPathPart :: forall a. Maybe (VarPathPart a) -> VarPathPart a -> String
printMapVarPathPart prev (VarPathPart { name, args } _) =
  combine
    [ dot
    , printMapVarPartName name
    , printMapArgs args
    ]
  where
  dot = guard (isJust prev) $ atStart unit "."

printVarPartName :: forall a. VarPartName a -> String
printVarPartName = printMapVarPartName

printMapVarPartName :: forall a. VarPartName a -> String
printMapVarPartName = case _ of
  VarPartNameGqlName gqlName _ ->
    atStart unit gqlName
  VarPartNameParent _ ->
    atStart unit parent
  VarPartNameRoot _ ->
    atStart unit root

printMapArgs :: forall a. Maybe (Args a) -> String
printMapArgs = case _ of
  Nothing -> empty
  Just Nil -> empty
  Just args@((Argument _) : _) ->
    combine $
      pure (atEnd unit "(")
        <> mapWithPrevious printMapArg args
        <> pure (atEnd unit ")")

printMapArg :: forall a. Maybe (Argument a) -> Argument a -> String
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
      Just (Argument _) -> atStart unit ","

printMapArgName :: forall a. ArgName a -> String
printMapArgName (ArgName name _) = combine
  [ atStart unit name
  , atStart unit ": "
  ]

