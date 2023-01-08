module GraphQL.Templater.TypeCheck
  ( TypeErrorWithPath(..)
  , PositionedError
  , TypeError(..)
  , getTypeErrorsFromTree
  ) where

import Prelude

import Control.Monad.State (execState, modify_)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST (Arguments, ArgumentsDefinition)
import Data.Lazy (force)
import Data.List (List(..), uncons, (:))
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.JsonPos (JsonPos(..), NormalizedJsonPos(..), normalizePos)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), TypeMap)



data TypeErrorWithPath a = TypeErrorWithPath TypeError (List (NormalizedJsonPos a)) a

type PositionedError = TypeErrorWithPath Positions

derive instance Generic (TypeErrorWithPath a) _
derive instance Eq a => Eq (TypeErrorWithPath a)
derive instance Functor TypeErrorWithPath
instance Show a => Show (TypeErrorWithPath a) where
  show = genericShow
data TypeError
  = FieldNotFound
  | NotObject
  | NotNode
  | NotList

derive instance Generic TypeError _
derive instance Eq TypeError
instance Show TypeError where
  show = genericShow
  
getTypeErrorsFromTree :: GqlTypeTree -> List AstPos -> List PositionedError
getTypeErrorsFromTree typeTree asts' = _.errors $ execState (checkAsts asts') initialState
  where
  initialState
    :: { errors :: List PositionedError
       , path :: List (JsonPos Positions)
       }
  initialState =
    { errors: Nil
    , path: Nil
    }

  checkAsts :: List AstPos -> _
  checkAsts asts =
    case uncons asts of
      Nothing -> pure unit
      Just { head: ast, tail } -> do
        case ast of
          Var (VarPath v _) _p -> do
            modify_ \st ->
              let
                path = normalizePos $ varPathToPositionWoArgs v <> st.path
              in
                st
                  { errors = getVarPathErrors typeTree (getPos $ NonEmpty.head v) path path
                      <> st.errors
                  }
            checkAsts tail
          Each (VarPath v _) inner _p -> do
            checkAsts tail
            modify_ \st ->
              let
                path' = varPathToPositionWoArgs v <> st.path
                path = normalizePos path'
              in
                st
                  { errors = getEachPathErrors typeTree (getPos $ NonEmpty.head v) path path
                      <> st.errors
                  , path = path'
                  }
            checkAsts inner

          Text _ _ -> checkAsts tail

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
      notList = pure $ TypeErrorWithPath NotList fullPath positions

      --  { error: NotList, positions, path: fullPath }

      go :: GqlTypeTree -> List PositionedError
      go = case _ of
        ObjectType _ -> notList
        NonNull t -> go t
        ListType _t -> Nil
        Node _ -> notList
        GqlUndefined -> notList

    Cons (Key k p) rest ->
      getTypeMap k p fullPath types
        # either pure
            ( lookupType k p fullPath >>> either pure \{ args, returns } ->
                getEachPathErrors returns p fullPath rest
            )

    Cons (Index _i p) rest -> go types
      where
      notList = pure $ TypeErrorWithPath NotList fullPath positions
      go = case _ of
        ListType t -> getEachPathErrors t p fullPath rest
        NonNull t -> go t
        ObjectType _ -> notList
        Node _ -> notList
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
      notNode = pure $ TypeErrorWithPath NotNode fullPath positions

      go :: GqlTypeTree -> List PositionedError
      go = case _ of
        ObjectType _ -> notNode
        NonNull t -> go t
        ListType _t -> notNode
        Node _ -> Nil
        GqlUndefined -> notNode

    Cons (Key k p) rest ->
      getTypeMap k p fullPath types
        # either pure
            ( lookupType k p fullPath >>> either pure \{ args, returns } ->
                getVarPathErrors returns p fullPath rest
            )

    Cons (Index _i p) rest -> go types
      where
      notList = pure $ TypeErrorWithPath NotList fullPath positions
      -- pure { error: NotList, positions: p, path: fullPath }
      go = case _ of
        ListType t -> getVarPathErrors t p fullPath rest
        NonNull t -> go t
        ObjectType _ -> notList
        Node _ -> notList
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
    Nothing -> Left $ TypeErrorWithPath FieldNotFound path p
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
    ListType t -> getTypeMap k p path t
    Node _ -> Left $ TypeErrorWithPath NotObject path p
    GqlUndefined -> Left $ TypeErrorWithPath NotObject path p

  varPathToPositionWoArgs :: forall f a. Foldable f => f (VarPathPart a) -> List (JsonPos a)
  varPathToPositionWoArgs path = foldl step Nil path
    where
    step res (VarPathPart { name } _) = case name of
      VarPartNameRoot a -> Root a : res
      VarPartNameParent a -> Parent a : res
      VarPartNameGqlName gqlName a ->
        (Pos $ Key (gqlName) a)
          : res