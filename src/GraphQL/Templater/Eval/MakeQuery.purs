module GraphQL.Templater.Eval.MakeQuery (getAlias, nilArgs, toGqlString) where

import Prelude

import Data.Foldable (foldl)
import Data.GraphQL.AST (Arguments(..))
import Data.GraphQL.AST as GqlAst
import Data.GraphQL.AST.Print (printAst)
import Data.Hashable (hash)
import Data.List (List(..), null, reverse, tail, (:))
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (drop, take)
import Data.Tuple (Tuple(..))
import GraphQL.Templater.Ast (Arg(..), ArgName(..), Args, Ast(..), Value(..), VarPartName(..), VarPath(..), VarPathPart(..))

newtype SelectionTree a =
  SelectionTree (Selections a)

type Selections a = Map { name :: String, args :: (Args a) } (SelectionTree a)

toGqlString :: forall a. Ord a => List (Ast a) -> Maybe String
toGqlString = astToGqlOperationDefinition >>> map printAst

astToGqlOperationDefinition :: forall a. Ord a => List (Ast a) -> Maybe GqlAst.OperationDefinition
astToGqlOperationDefinition =
  makeSelections
    >>> map (SelectionTree >>> addTypenames >>> toGqlOperationDefinition)

toGqlOperationDefinition :: forall a. SelectionTree a -> GqlAst.OperationDefinition
toGqlOperationDefinition (SelectionTree sels) = GqlAst.OperationDefinition_OperationType
  { directives: Nothing
  , name: Nothing
  , operationType: GqlAst.Query
  , selectionSet: toGqlSelectionSet (SelectionTree sels)
  , variableDefinitions: Nothing
  }

toGqlSelectionSet :: forall a. SelectionTree a -> GqlAst.SelectionSet
toGqlSelectionSet (SelectionTree sels) = GqlAst.SelectionSet
  $ Map.toUnfoldable sels <#> \(Tuple { name, args } (SelectionTree tree)) ->
      GqlAst.Selection_Field $ GqlAst.Field
        { alias: getAlias name args
        , arguments:
            if null args then
              Nothing
            else
              Just $ Arguments $ map toAstArg args
        , directives: Nothing
        , name
        , selectionSet: if Map.isEmpty tree then Nothing else Just $ toGqlSelectionSet (SelectionTree tree)
        }

  where
  toAstArg :: Arg a -> GqlAst.Argument
  toAstArg (Arg { name: (ArgName name _), value: (Value value _) } _) = GqlAst.Argument
    { name
    , value
    }

getAlias ∷ forall a. String → Args a → Maybe String
getAlias name args =
  if null args then
    Nothing
  else
    Just $ name <> "__" <> removeNegation (show $ hash $ show $ map (const unit) <$> args)
  where
  removeNegation str = if take 1 str == "-" then "N" <> drop 1 str else str

makeSelections :: forall a. Ord a => List (Ast a) -> Maybe (Selections a)
makeSelections topLevelAsts =
  if not hasQuery topLevelAsts then
    Nothing
  else
    Just $ go Nil Map.empty topLevelAsts

  where

  go :: List (Tuple (Args a) String) -> (Selections a) -> List (Ast a) -> (Selections a)
  go ancestors = foldl (step ancestors)

  step :: List (Tuple (Args a) String) -> (Selections a) -> (Ast a) -> (Selections a)
  step ancestors res = case _ of
    Var (VarPath v _) _ -> normalizeAndInsertPath false ancestors res v
    Each (VarPath v _) ast _ ->
      go (normalizePath ancestors (NonEmpty.toList v))
        (normalizeAndInsertPath true ancestors res v)
        ast
    Text _ _ -> res

  normalizeAndInsertPath withTypename ancestors res v =
    let
      path = normalizePath ancestors (NonEmpty.toList v)
    in
      insertPath withTypename res (reverse path)

  insertPath :: Boolean -> (Selections a) -> List (Tuple (Args a) String) -> (Selections a)
  insertPath withTypename sels = case _ of
    Nil -> sels
    Tuple args name : rest ->
      let
        handleCollision (SelectionTree tree) = SelectionTree $ insertPath withTypename tree rest
      in
        Map.alter
          ( Just
              <<< handleCollision
              <<< fromMaybe (SelectionTree if withTypename then typenameMap else Map.empty)
          )
          { name, args }
          sels

hasQuery :: forall a. List (Ast a) -> Boolean
hasQuery = foldl step false
  where
  step res = case _ of
    Var _ _ -> true
    Each _ _ast _ -> true
    Text _ _ -> res

normalizePath :: forall a. List (Tuple (Args a) String) -> List (VarPathPart a) -> List (Tuple (Args a) String)
normalizePath res = case _ of
  VarPathPart { name: (VarPartNameRoot _) } _ : rest -> normalizePath Nil rest
  VarPathPart { name: (VarPartNameParent _) } _ : rest -> normalizePath (fromMaybe Nil $ tail res) rest
  VarPathPart { name: (VarPartNameGqlName gqlName _), args } _ : rest ->
    normalizePath (Tuple (fromMaybe Nil args) gqlName : res) rest
  _ -> res

addTypenames :: forall a. Ord a => SelectionTree a -> SelectionTree a
addTypenames selTree@(SelectionTree s) =
  if Map.isEmpty s then
    selTree
  else
    SelectionTree
      $ Map.union typenameMap s
          <#> addTypenames

typenameMap :: forall a. Selections a
typenameMap = Map.singleton { args: Nil, name: "__typename" } (SelectionTree Map.empty)

nilArgs :: Arguments
nilArgs = Arguments Nil
