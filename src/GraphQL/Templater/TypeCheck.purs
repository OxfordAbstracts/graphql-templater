module GraphQL.Templater.TypeCheck where

import Prelude

import Control.Monad.State (execState, modify_)
import Data.Either (Either(..), either)
import Data.GraphQL.AST (Arguments(..), ArgumentsDefinition)
import Data.Lazy (force)
import Data.List (List(..), uncons)
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Maybe (Maybe(..))
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPath(..), VarPathPart(..))
import GraphQL.Templater.JsonPos (JsonPos, NormalizedJsonPos(..), normalizePos, varPathToPosition)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), TypeMap)

data TypeError
  = FieldNotFound
  | NotObject
  | NotNode
  | NotList

type PositionedError =
  { error :: TypeError
  , path :: List (NormalizedJsonPos Positions)
  , positions :: Positions
  }

getTypeErrorsFromTree :: GqlTypeTree -> List AstPos -> List PositionedError
getTypeErrorsFromTree typeTree asts' = _.errors $ execState (goAsts asts') initialState
  where
  initialState
    :: { errors :: List PositionedError
       , path :: List (JsonPos Positions)
       }
  initialState =
    { errors: Nil
    , path: Nil
    }

  goAsts :: List AstPos -> _
  goAsts asts =
    case uncons asts of
      Nothing -> pure unit
      Just { head: ast, tail } -> do
        case ast of
          Var (VarPath v _) _p -> do
            modify_ \st ->
              let
                path = normalizePos $ varPathToPosition v <> st.path
              in
                st
                  { errors = getVarPathErrors typeTree (getPos $ NonEmpty.head v) path path
                      <> st.errors
                  }
          Each (VarPath v _) inner _p -> do
            goAsts tail
            modify_ \st ->
              let
                path' = varPathToPosition v <> st.path
                path = normalizePos path'
              in
                st
                  { errors = getEachPathErrors typeTree (getPos $ NonEmpty.head v) path path
                      <> st.errors
                  , path = path'
                  }
            goAsts inner

          Text _ _ -> goAsts tail

  getPos (VarPathPart _ p) = p

  getEachPathErrors
    :: GqlTypeTree
    -> Positions
    -> List (NormalizedJsonPos Positions)
    -> List (NormalizedJsonPos Positions)
    -> List PositionedError
  getEachPathErrors types positions fullPath = case _ of

    Nil -> go types
      where
      notList = pure { error: NotList, positions, path: fullPath }

      go :: GqlTypeTree -> List PositionedError
      go = case _ of
        ObjectType _ -> notList
        NonNull t -> go t
        ListType _t -> Nil
        Node -> notList
        GqlUndefined -> notList

    Cons (Key k p) rest ->
      getTypeMap k p fullPath types
        # either pure
            ( lookupType k p fullPath >>> either pure \{ args, returns } ->
                getEachPathErrors returns p fullPath rest
            )

    Cons (Index i p) rest -> go types
      where
      notList = pure { error: NotList, positions: p, path: fullPath }
      go = case _ of
        ListType t -> getEachPathErrors t p fullPath rest
        NonNull t -> go t
        ObjectType _ -> notList
        Node -> notList
        GqlUndefined -> notList

  getVarPathErrors
    :: GqlTypeTree
    -> Positions
    -> List (NormalizedJsonPos Positions)
    -> List (NormalizedJsonPos Positions)
    -> List PositionedError
  getVarPathErrors types positions fullPath = case _ of

    Nil -> go types
      where
      notNode = pure { error: NotNode, positions, path: fullPath }

      go :: GqlTypeTree -> List PositionedError
      go = case _ of
        ObjectType _ -> notNode
        NonNull t -> go t
        ListType _t -> notNode
        Node -> Nil
        GqlUndefined -> notNode

    Cons (Key k p) rest ->
      getTypeMap k p fullPath types
        # either pure
            ( lookupType k p fullPath >>> either pure \{ args, returns } ->
                getVarPathErrors returns p fullPath rest
            )

    Cons (Index i p) rest -> go types
      where
      notList = pure { error: NotList, positions: p, path: fullPath }
      go = case _ of
        ListType t -> getVarPathErrors t p fullPath rest
        NonNull t -> go t
        ObjectType _ -> notList
        Node -> notList
        GqlUndefined -> notList

  typeCheckArguments :: ArgumentsDefinition -> Arguments -> List PositionedError
  typeCheckArguments argsDef args = Nil

  lookupType
    :: String
    -> Positions
    -> List (NormalizedJsonPos Positions)
    -> TypeMap
    -> Either PositionedError
         { args :: Maybe ArgumentsDefinition
         , returns :: GqlTypeTree
         }
  lookupType k p path t = case force <$> Map.lookup k t of
    Nothing -> Left { error: FieldNotFound, positions: p, path }
    Just ret -> Right ret

  getTypeMap
    :: String
    -> Positions
    -> List (NormalizedJsonPos Positions)
    -> GqlTypeTree
    -> Either PositionedError TypeMap
  getTypeMap k p path = case _ of
    ObjectType obj -> Right obj
    NonNull t -> getTypeMap k p path t
    ListType _t -> Left { error: NotObject, positions: p, path }
    Node -> Left { error: NotObject, positions: p, path }
    GqlUndefined -> Left { error: NotObject, positions: p, path }
