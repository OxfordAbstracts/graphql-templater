module GraphQL.Templater.TypeCheck
  ( TypeErrorWithPath(..)
  , PositionedError
  , TypeError(..)
  , getTypeErrorsFromTree
  ) where

import Prelude

import Control.Monad.State (execState, get, modify_)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldl)
import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST (Arguments)
import Data.Lazy (force)
import Data.List (List(..), uncons, (:))
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (fst)
import Debug (spy, spyWith)
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.JsonPos (JsonPos(..), NormalizedJsonPos(..), normalizePos)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeCheck.Arguments (ArgTypeError, typeCheckArguments)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..))

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
  | ArgTypeError ArgTypeError

derive instance Generic TypeError _
derive instance Eq TypeError
instance Show TypeError where
  show = genericShow

getTypeErrorsFromTree :: GqlTypeTree -> List AstPos -> List PositionedError
getTypeErrorsFromTree typeTree asts' = map (map _.pos) $ _.errors $ execState (checkAsts asts') initialState
  where
  initialState
    :: { errors :: List (TypeErrorWithPath PosAndArgs)
       , path :: List (JsonPos PosAndArgs)
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
                path = normalizePos $
                  varPathToPosAndArgs v <> st.path
              in
                st
                  { errors = spy "var path errors" $
                      getVarPathErrors path (getStartPos v) typeTree path
                        <> st.errors
                  }
            checkAsts tail
          Each (VarPath v _) inner _p -> do
            originalSt <- get
            modify_ \st ->
              let
                path' = varPathToPosAndArgs (spyWith "v" show v) <> st.path
                path = normalizePos path'
              in
                st
                  { errors = spy "each path errors" $
                      getEachPathErrors (spyWith "each path" show path) (getStartPos v) typeTree path
                        <> st.errors
                  , path = path'
                  }
            checkAsts inner
            modify_ \st -> st { path = originalSt.path }
            checkAsts tail

          Text _ _ -> checkAsts tail

  getStartPos v = { pos: getPos $ NonEmpty.head v, args: Nothing }

  getPos (VarPathPart _ p) = p

  getEachPathErrors fullPath positions = getErrors atEnd fullPath positions
    where
    notList = pure $ TypeErrorWithPath NotList fullPath positions

    atEnd = case _ of
      ObjectType _ -> notList
      NonNull t -> atEnd t
      ListType _t -> Nil
      Node _ -> notList
      GqlUndefined -> notList

  getVarPathErrors
    :: List (NormalizedJsonPos PosAndArgs)
    -> PosAndArgs
    -> GqlTypeTree
    -> List (NormalizedJsonPos PosAndArgs)
    -> List (TypeErrorWithPath PosAndArgs)
  getVarPathErrors fullPath positions = getErrors atEnd fullPath positions
    where
    notNode = pure $ TypeErrorWithPath NotNode fullPath positions

    atEnd = case _ of
      ObjectType _ -> notNode
      NonNull t -> atEnd t
      ListType _t -> notNode
      Node _ -> Nil
      GqlUndefined -> notNode

  getErrors
    :: (GqlTypeTree -> List (TypeErrorWithPath PosAndArgs))
    -> List (NormalizedJsonPos PosAndArgs)
    -> PosAndArgs
    -> GqlTypeTree
    -> List (NormalizedJsonPos PosAndArgs)
    -> List (TypeErrorWithPath PosAndArgs)
  getErrors atPathEnd fullPath positions types = case _ of

    Nil -> atPathEnd types

    Cons (Key k p) rest ->
      getTypeMap k p fullPath types
        # either pure
            ( lookupType k p fullPath >>> either pure \{ args, returns } ->
                (argTypeErrorToTypeError fullPath p <$> typeCheckArguments args p.args) <>
                  getErrors atPathEnd fullPath p returns rest
            )

    Cons (Index _i p) rest -> go types
      where
      notList = pure $ TypeErrorWithPath NotList fullPath positions

      go = case _ of
        ListType t -> getErrors atPathEnd fullPath p t rest
        NonNull t -> go t
        ObjectType _ -> notList
        Node _ -> notList
        GqlUndefined -> notList

  lookupType k p path t = case force <$> Map.lookup k t of
    Nothing -> Left $ TypeErrorWithPath FieldNotFound path p
    Just ret -> Right ret

  getTypeMap k p path = case _ of
    ObjectType obj -> Right obj
    NonNull t -> getTypeMap k p path t
    ListType t -> getTypeMap k p path t
    Node _ -> Left $ TypeErrorWithPath NotObject path p
    GqlUndefined -> Left $ TypeErrorWithPath NotObject path p

varPathToPosAndArgs :: forall f a. Foldable f => f (VarPathPart a) -> List (JsonPos { pos :: a, args :: Maybe Arguments })
varPathToPosAndArgs path = foldl step Nil path
  where
  step res (VarPathPart { name, args: args_ } _) = case name of
    VarPartNameRoot pos -> Root { pos, args } : res
    VarPartNameParent pos -> Parent { pos, args } : res
    VarPartNameGqlName gqlName pos ->
      (Pos $ Key (gqlName) { pos, args })
        : res
    where
    args = map fst args_

type PosAndArgs = { pos :: Positions, args :: Maybe Arguments }

argTypeErrorToTypeError :: forall a. (List (NormalizedJsonPos a)) -> a -> ArgTypeError -> TypeErrorWithPath a
argTypeErrorToTypeError pos a argTypeError = TypeErrorWithPath (ArgTypeError argTypeError) pos a
