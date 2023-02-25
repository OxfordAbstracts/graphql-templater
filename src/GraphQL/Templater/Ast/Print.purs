module GraphQL.Templater.Ast.Print
  ( printEach
  , printMapVarPath
  , printMapVarPathPart
  , printPositioned
  , printSingleAstPositioned
  , printUnpositioned
  , printVarPartName
  , printWith
  ) where

import Prelude

import Data.List (List(..), fold, foldMap, (:))
import Data.List.Types (toList)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (guard)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..), Args)
import GraphQL.Templater.Ast.Argument (ArgName(..), Argument(..))
import GraphQL.Templater.Ast.Argument.Print (printValue)
import GraphQL.Templater.Ast.PrintUtils (mapWithPrevious)
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
printUnpositioned asts = fold $ map printMapTemplateAst asts

-- -- asUnpositioned fn = displayPrintResult <<< fn <<< map dummyPositions

printMapTemplateAsts :: forall a. List (Ast a) -> String
printMapTemplateAsts asts = fold $ map printMapTemplateAst asts

printMapTemplateAst :: forall a. Ast a -> String
printMapTemplateAst = case _ of
  Var varPath _ ->
    fold
      [ openVar
      , printMapVarPath varPath
      , closeVar
      ]
  Each varPath inner _ _ ->
    printEach varPath inner

  With varPath inner _ _ ->
    printWith varPath inner

  Text text _ ->
    text

printEach :: forall a. VarPath a -> List (Ast a) -> String
printEach varPath@(VarPath _ _) inner =
  fold
    [ eachOpen
    , printMapVarPath varPath
    , closeVar
    , printMapTemplateAsts inner
    , eachClose
    ]

printWith :: forall a. VarPath a -> List (Ast a) -> String
printWith varPath inner =
  fold
    [ withOpen
    , printMapVarPath varPath
    , closeVar
    , printMapTemplateAsts inner
    , withClose
    ]

printMapVarPath :: forall a. VarPath a -> String
printMapVarPath (VarPath path _) = fold $ mapWithPrevious printMapVarPathPart (toList path)

printMapVarPathPart :: forall a. Maybe (VarPathPart a) -> VarPathPart a -> String
printMapVarPathPart prev (VarPathPart { name, args } _) =
  fold
    [ dot
    , printMapVarPartName name
    , printMapArgs args
    ]
  where
  dot = guard (isJust prev) $ "."

printVarPartName :: forall a. VarPartName a -> String
printVarPartName = printMapVarPartName

printMapVarPartName :: forall a. VarPartName a -> String
printMapVarPartName = case _ of
  VarPartNameGqlName gqlName _ ->
    gqlName
  VarPartNameParent _ ->
    parent
  VarPartNameRoot _ ->
    root

printMapArgs :: forall a. Maybe (Args a) -> String
printMapArgs = case _ of
  Nothing -> ""
  Just Nil -> ""
  Just args@(_ : _) ->
    fold $
      pure "("
        <> mapWithPrevious printMapArg args
        <> pure ")"

printMapArg :: forall a. Maybe (Argument a) -> Argument a -> String
printMapArg prev (Argument { name, value }) =
  fold
    [ comma
    , printMapArgName name
    , printValue value
    ]
  where
  comma =
    case prev of
      Nothing -> ""
      Just (Argument _) -> ","

printMapArgName :: forall a. ArgName a -> String
printMapArgName (ArgName name _) = fold
  [ name
  , ": "
  ]

