module GraphQL.Templater.Ast.Suggest where

import Prelude

import Data.Lazy (force)
import Data.List (List(..), mapMaybe, (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import GraphQL.Templater.Ast (Ast(..), VarPath(..))
import GraphQL.Templater.JsonPos (NormalizedJsonPos(..), normalizePos, varPathToPosition)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), TypeMap, getTypeAtPath, getTypeMapFromTree)

data SuggestionState
  = Start
  | InHash
  | InEach VarPathState
  | InWith VarPathState
  | InVar VarPathState

type VarPathState =
  { current :: Array String
  , possible :: Array String
  }

-- getStartingState :: Int -> String -> List (Ast Positions) -> GqlTypeTree -> Maybe SuggestionState
-- getStartingState idx template asts typeTree = Nothing

getStartingSuggestions
  :: Int
  -> List (Ast Positions)
  -> GqlTypeTree
  -> { eaches :: List { field :: String, description :: Maybe String }
     , vars :: List { field :: String, description :: Maybe String }
     }
getStartingSuggestions idx asts typeTree = getTypeMapAt idx asts typeTree
  # maybe mempty
      ( map force >>> Map.toUnfoldable >>> List.partition isList >>> \{ yes, no } ->
          { eaches: map getSuggestion yes
          , vars: map getSuggestion no
          }
      )
  where
  isList = snd >>> _.returns >>> case _ of
    ListType _ -> true
    _ -> false

  getSuggestion (field /\ { description }) = { field, description }

getTypeMapAt :: Int -> List (Ast Positions) -> GqlTypeTree -> Maybe TypeMap
getTypeMapAt idx asts typeTree = getTypeMapFromTree =<< getTypeAtPath keyPath typeTree
  where
  keyPath :: List String
  keyPath = getPathAt idx asts

getPathAt :: Int -> List (Ast Positions) -> List String
getPathAt idx asts = getJsonPosPathAt idx asts # mapMaybe case _ of
  Key key _ -> Just key.name
  _ -> Nothing

getJsonPosPathAt :: Int -> List (Ast Positions) -> List (NormalizedJsonPos Positions)
getJsonPosPathAt = map normalizePos <<< go Nil
  where
  go path idx asts = case asts of
    Nil -> path
    head : tail -> case head of
      Each (VarPath varPath _) inner open close
        | isWithin idx open close ->
            go (varPathToPosition varPath <> path) idx inner
      With (VarPath varPath _) inner open close
        | isWithin idx open close ->
            go (varPathToPosition varPath <> path) idx inner
      _ -> go path idx tail

getAstAt :: Int -> List (Ast Positions) -> Maybe (Ast Positions)
getAstAt idx = case _ of
  Nil -> Nothing
  head : tail ->
    if isAtIdx idx head then
      Just head
    else case head of
      Each _ inner open close
        | isWithin idx open close -> getAstAt idx inner
      With _ inner open close
        | isWithin idx open close -> getAstAt idx inner
      _ -> getAstAt idx tail

isAtIdx :: Int -> Ast Positions -> Boolean
isAtIdx idx = case _ of
  Each _ _ open close ->
    (idx > getStartIdx open && idx < getEndIdx open)
      || (idx > getStartIdx close && idx < getEndIdx close)
  With _ _ open close ->
    (idx > getStartIdx open && idx < getEndIdx open)
      || (idx > getStartIdx close && idx < getEndIdx close)
  Var _ pos -> idx >= getStartIdx pos && idx < getEndIdx pos
  Text _ pos -> idx >= getStartIdx pos && idx <= getEndIdx pos

isWithin
  :: Int
  -> Positions
  -> Positions
  -> Boolean
isWithin idx open close = (idx >= getEndIdx open)
  && (idx <= getStartIdx close)

getStartIdx :: Positions -> Int
getStartIdx { start } = start

getEndIdx :: Positions -> Int
getEndIdx { end } = end

