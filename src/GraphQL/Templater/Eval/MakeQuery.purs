module GraphQL.Templater.Eval.MakeQuery (getAlias, nilArgs, toGqlString) where

import Prelude

import Data.Foldable (foldl)
import Data.GraphQL.AST (Arguments(..))
import Data.GraphQL.AST as GqlAst
import Data.GraphQL.AST.Print (printAst)
import Data.Hashable (hash)
import Data.List (List(..), reverse, tail, (:))
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (drop, take)
import Data.Tuple (Tuple(..), fst)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..))

newtype SelectionTree =
  SelectionTree Selections

type Selections = Map { name :: String, args :: Arguments } SelectionTree

toGqlString :: forall a. List (Ast a) -> Maybe String
toGqlString = astToGqlOperationDefinition >>> map printAst

astToGqlOperationDefinition :: forall a. List (Ast a) -> Maybe GqlAst.OperationDefinition
astToGqlOperationDefinition =
  makeSelections
    >>> map (SelectionTree >>> addTypenames >>> toGqlOperationDefinition)

toGqlOperationDefinition :: SelectionTree -> GqlAst.OperationDefinition
toGqlOperationDefinition (SelectionTree sels) = GqlAst.OperationDefinition_OperationType
  { directives: Nothing
  , name: Nothing
  , operationType: GqlAst.Query
  , selectionSet: toGqlSelectionSet (SelectionTree sels)
  , variableDefinitions: Nothing
  }

toGqlSelectionSet :: SelectionTree -> GqlAst.SelectionSet
toGqlSelectionSet (SelectionTree sels) = GqlAst.SelectionSet
  $ Map.toUnfoldable sels <#> \(Tuple { name, args } (SelectionTree tree)) ->
      GqlAst.Selection_Field $ GqlAst.Field
        { alias: getAlias name args
        , arguments: if args == nilArgs then Nothing else Just args
        , directives: Nothing
        , name
        , selectionSet: if Map.isEmpty tree then Nothing else Just $ toGqlSelectionSet (SelectionTree tree)
        }

getAlias ∷ String → Arguments → Maybe String
getAlias name args = if args == nilArgs then Nothing else Just $ name <> "__" <> removeNegation (show $ hash args)
  where
  removeNegation str = if take 1 str == "-" then "N" <> drop 1 str else str

makeSelections :: forall a. List (Ast a) -> Maybe Selections
makeSelections topLevelAsts =
  if noVariables topLevelAsts then
    Nothing
  else
    Just $ go Nil Map.empty topLevelAsts

  where

  go :: List (Tuple Arguments String) -> Selections -> List (Ast a) -> Selections
  go ancestors = foldl (step ancestors)

  step :: List (Tuple Arguments String) -> Selections -> (Ast a) -> Selections
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

  insertPath :: Boolean -> Selections -> List (Tuple Arguments String) -> Selections
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

noVariables :: forall a. List (Ast a) -> Boolean
noVariables = foldl step true
  where
  step res = case _ of
    Var _ _ -> false
    Each _ ast _ -> res && noVariables ast
    Text _ _ -> res

normalizePath :: forall a. List (Tuple Arguments String) -> List (VarPathPart a) -> List (Tuple Arguments String)
normalizePath res = case _ of
  VarPathPart { name: (VarPartNameRoot _) } _ : rest -> normalizePath Nil rest
  VarPathPart { name: (VarPartNameParent _) } _ : rest -> normalizePath (fromMaybe Nil $ tail res) rest
  VarPathPart { name: (VarPartNameGqlName gqlName _), args } _ : rest ->
    normalizePath (Tuple (getPartArgs args) gqlName : res) rest
  _ -> res

getPartArgs ∷ ∀ (a ∷ Type). Maybe (Tuple Arguments a) → Arguments
getPartArgs = maybe nilArgs fst

addTypenames :: SelectionTree -> SelectionTree
addTypenames selTree@(SelectionTree s) =
  if Map.isEmpty s then
    selTree
  else
    SelectionTree
      $ Map.union typenameMap s
          <#> addTypenames

typenameMap :: Selections
typenameMap = Map.singleton { args: nilArgs, name: "__typename" } (SelectionTree Map.empty)

nilArgs :: Arguments
nilArgs = Arguments Nil
