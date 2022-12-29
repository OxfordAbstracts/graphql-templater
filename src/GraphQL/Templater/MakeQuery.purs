module GraphQL.Templater.MakeQuery where

import Prelude

import Data.Foldable (foldl, lookup)
import Data.FunctorWithIndex (mapWithIndex)
import Data.GraphQL.AST (Arguments(..))
import Data.GraphQL.AST as GqlAst
import Data.GraphQL.AST.Print (printAst)
import Data.Hashable (hash)
import Data.List (List(..), reverse, tail, (:))
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..))

data SelectionTree =
  SelectionTree Selections

type Selections = Map { name :: String, args :: Arguments } SelectionTree

toGqlString :: forall a. List (Ast a) -> String
toGqlString = makeSelections
  >>> SelectionTree
  >>> toGqlOperationDefinition
  >>> printAst

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
        { alias: if args == nilArgs then Nothing else Just $ name <> "__" <> show (hash args)
        , arguments: if args == nilArgs then Nothing else Just args
        , directives: Nothing
        , name
        , selectionSet: if Map.isEmpty tree then Nothing else Just $ toGqlSelectionSet (SelectionTree tree)
        }

makeSelections :: forall a. List (Ast a) -> Selections
makeSelections = go Nil Map.empty
  where
  go :: List (Tuple Arguments String) -> _ -> List (Ast a) -> Selections
  go ancestors = foldl (step ancestors)

  step :: List (Tuple Arguments String) -> Selections -> (Ast a) -> Selections
  step ancestors res = case _ of
    Var (VarPath v _) _ -> normalizeAndInsertPath ancestors res v
    Each (VarPath v _) ast _ ->
      go (normalizePath ancestors (pure $ NonEmpty.head v))
        (normalizeAndInsertPath ancestors res v)
        ast
    Text _ _ -> res

  normalizeAndInsertPath ancestors res v =
    let
      path = normalizePath ancestors (NonEmpty.toList v)
    in
      insertPath res (reverse path)

  insertPath :: Selections -> List (Tuple Arguments String) -> Selections
  insertPath sels = case _ of
    Nil -> sels
    Tuple args name : rest ->
      let
        handleCollision (SelectionTree tree) = SelectionTree $ insertPath tree rest
      in
        Map.alter
          ( Just <<< handleCollision
              <<< fromMaybe (SelectionTree Map.empty)
          )
          { name, args }
          sels

  normalizePath :: List (Tuple Arguments String) -> List (VarPathPart a) -> List (Tuple Arguments String)
  normalizePath res = case _ of
    VarPathPart { name: (VarPartNameRoot _) } _ : rest -> normalizePath Nil rest
    VarPathPart { name: (VarPartNameParent _) } _ : rest -> normalizePath (fromMaybe Nil $ tail res) rest
    VarPathPart { name: (VarPartNameGqlName gqlName _), args } _ : rest ->
      normalizePath (Tuple (getPartArgs args) gqlName : res) rest
    _ -> res

  getPartArgs = maybe nilArgs fst

nilArgs :: Arguments
nilArgs = Arguments Nil
