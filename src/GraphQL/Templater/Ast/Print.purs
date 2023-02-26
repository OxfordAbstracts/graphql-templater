module GraphQL.Templater.Ast.Print
  ( printEach
  , printPositioned
  , printPositionedMb
  , printSingleAstPositioned
  , printUnpositioned
  , printVarPartName
  , printVarPath
  , printVarPathPart
  , printWith
  ) where

import Prelude

import Data.List (List(..), fold, foldMap, (:))
import Data.List.Types (toList)
import Data.Maybe (Maybe(..), isJust, maybe')
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

printPositionedMb :: List (Ast (Maybe Positions)) -> String
printPositionedMb asts = foldMap printSingleAstPositionedMb asts

printSingleAstPositionedMb :: Ast (Maybe Positions) -> String
printSingleAstPositionedMb = case _ of
  Text text _ -> text
  Var varPath pos -> maybe' (\_ -> printVar varPath) _.str pos
  Each varPath inner open close ->
    maybe' (\_ -> printOpenEach varPath) _.str open
      <> printPositionedMb inner
      <> maybe' (\_ -> eachClose) _.str close
  With varPath inner open close ->
    maybe' (\_ -> printOpenWith varPath) _.str open
      <> printPositionedMb inner
      <> maybe' (\_ -> withClose) _.str close

-- -- | Print an AST, discarding the original positions of the tokens.
printUnpositioned :: forall a. List (Ast a) -> String
printUnpositioned asts = fold $ map printMapTemplateAst asts

printMapTemplateAsts :: forall a. List (Ast a) -> String
printMapTemplateAsts asts = fold $ map printMapTemplateAst asts

printMapTemplateAst :: forall a. Ast a -> String
printMapTemplateAst = case _ of
  Var varPath _ ->
    fold
      [ openVar
      , printVarPath varPath
      , closeVar
      ]
  Each varPath inner _ _ ->
    printEach varPath inner

  With varPath inner _ _ ->
    printWith varPath inner

  Text text _ ->
    text

printVar :: forall a. VarPath a -> String
printVar varPath = fold
  [ openVar
  , printVarPath varPath
  , closeVar
  ]

printEach :: forall a. VarPath a -> List (Ast a) -> String
printEach varPath@(VarPath _ _) inner =
  fold
    [ printOpenEach varPath
    , printMapTemplateAsts inner
    , eachClose
    ]

printOpenEach :: forall a. VarPath a -> String
printOpenEach varPath = fold
  [ eachOpen
  , printVarPath varPath
  , closeVar
  ]

printWith :: forall a. VarPath a -> List (Ast a) -> String
printWith varPath inner =
  fold
    [ printOpenWith varPath
    , printMapTemplateAsts inner
    , withClose
    ]

printOpenWith :: forall a. VarPath a -> String
printOpenWith varPath = fold
  [ withOpen
  , printVarPath varPath
  , closeVar
  ]

printVarPath :: forall a. VarPath a -> String
printVarPath (VarPath path _) = fold $ mapWithPrevious printVarPathPart (toList path)

printVarPathPart :: forall a. Maybe (VarPathPart a) -> VarPathPart a -> String
printVarPathPart prev (VarPathPart { name, args } _) =
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

