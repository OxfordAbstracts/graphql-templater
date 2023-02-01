module GraphQL.Templater.Ast.Suggest where

import Prelude

import Data.Lazy (force)
import Data.List (List(..), mapMaybe, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Tuple (Tuple(..))
import GraphQL.Templater.Ast (Ast(..), VarPath(..))
import GraphQL.Templater.JsonPos (NormalizedJsonPos(..), normalizePos, varPathToPosition)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), TypeFieldValue, TypeMap, getTypeAtPath, getTypeMapFromTree)
import Parsing (Position(..))

suggestEaches :: Int -> List (Ast Positions) -> GqlTypeTree -> List String
suggestEaches idx asts typeTree =
  getTypeMapAt idx asts typeTree
    # maybe Nil (Map.toUnfoldable >>> mapMaybe getEach)
  where
  getEach :: Tuple String TypeFieldValue -> Maybe String
  getEach (Tuple name value) = guard (isList (force value).returns) $ Just name

  isList = case _ of
    ListType _ -> true
    _ -> false

getTypeMapAt :: Int -> List (Ast Positions) -> GqlTypeTree -> Maybe TypeMap
getTypeMapAt idx asts typeTree = getTypeMapFromTree =<< getTypeAtPath keyPath typeTree
  where
  keyPath :: List String
  keyPath = getPathAt idx asts

getPathAt :: Int -> List (Ast Positions) -> List String
getPathAt idx asts = getJsonPosPathAt idx asts # mapMaybe case _ of
  Key key _ -> Just key
  _ -> Nothing

getJsonPosPathAt :: Int -> List (Ast Positions) -> List (NormalizedJsonPos Positions)
getJsonPosPathAt = map normalizePos <<< go Nil
  where
  go path idx asts = case asts of
    Nil -> path
    head : tail -> case head of
      Each (VarPath varPath _) inner open close
        | isWithin open close ->
            go (varPathToPosition varPath <> path) idx inner
      With (VarPath varPath _) inner open close
        | isWithin open close ->
            go (varPathToPosition varPath <> path) idx inner
      _ -> go path idx tail
    where

    isWithin open close = (idx >= getEndIdx open)
      && (idx <= getStartIdx close)

  getStartIdx :: Positions -> Int
  getStartIdx { start: Position { index } } = index

  getEndIdx :: Positions -> Int
  getEndIdx { end: Position { index } } = index
