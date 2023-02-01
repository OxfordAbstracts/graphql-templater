module GraphQL.Templater.Ast.TypeCheck
  ( PosAndArgs
  , getTypeErrorsFromTree
  , varPathToPosAndArgs
  )
  where

import Prelude

import Control.Monad.State (execState, get, modify_)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldl, null)
import Data.Lazy (force)
import Data.List (List(..), uncons, (:))
import Data.List.NonEmpty as NonEmpty
import Data.Map as Map
import Data.Maybe (Maybe(..))
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPartName(..), VarPath(..), VarPathPart(..), Args)
import GraphQL.Templater.JsonPos (JsonPos(..), NormalizedJsonPos(..), normalizePos)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.Ast.TypeCheck.Arguments (typeCheckArguments)
import GraphQL.Templater.Ast.TypeCheck.Errors (ArgTypeError, PositionedError, TypeError(..), TypeErrorWithPath(..))
import GraphQL.Templater.TypeDefs (GqlTypeTree(..))

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
                  { errors =
                      getVarPathErrors path (getStartPos v) typeTree path
                        <> st.errors
                  }
            checkAsts tail
          Each (VarPath v _) inner _ _ -> do
            st <- get

            let
              path' = varPathToPosAndArgs v <> st.path
              path = normalizePos path'
              newErrors = getEachPathErrors path (getStartPos v) typeTree path

            modify_ \st_ ->
              st_
                { errors = newErrors <> st_.errors
                , path = path'
                }

            when (null newErrors) $
              checkAsts inner

            modify_ _ { path = st.path }

            checkAsts tail
          With (VarPath v _) inner _ _ -> do
            st <- get

            let
              path' = varPathToPosAndArgs v <> st.path
              path = normalizePos path'
              newErrors = getWithPathErrors path (getStartPos v) typeTree path

            modify_ \st_ ->
              st_
                { errors = newErrors <> st_.errors
                , path = path'
                }

            when (null newErrors) $
              checkAsts inner

            modify_ _ { path = st.path }

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

  getWithPathErrors fullPath positions = getErrors atEnd fullPath positions
    where
    notObject = pure $ TypeErrorWithPath NotObject fullPath positions

    atEnd = case _ of
      ObjectType _ -> Nil
      NonNull t -> atEnd t
      ListType _t -> notObject
      Node _ -> notObject

  getVarPathErrors
    :: List (NormalizedJsonPos PosAndArgs)
    -> PosAndArgs
    -> GqlTypeTree
    -> List (NormalizedJsonPos PosAndArgs)
    -> List (TypeErrorWithPath PosAndArgs)
  getVarPathErrors fullPath positions = getErrors atEnd fullPath positions
    where
    atEnd = case _ of
      ObjectType obj -> pure
        $ TypeErrorWithPath (ObjectWhenNodeExpected $ Map.keys obj) fullPath positions
      NonNull t -> atEnd t
      ListType t -> atEnd t
      Node _ -> Nil

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
                (argTypeErrorToTypeError fullPath p <$> typeCheckArguments args (map addNullArgs <$> p.args)) <>
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

    where
    addNullArgs = map { pos: _, args: Nothing }

  lookupType k p path t = case force <$> Map.lookup k t of
    Nothing -> Left $ TypeErrorWithPath (FieldNotFound $ Map.keys t) path p
    Just ret -> Right ret

  getTypeMap k p path = case _ of
    ObjectType obj -> Right obj
    NonNull t -> getTypeMap k p path t
    ListType t -> getTypeMap k p path t
    Node _ -> Left $ TypeErrorWithPath NotObject path p

varPathToPosAndArgs :: forall f a. Foldable f => f (VarPathPart a) -> List (JsonPos { pos :: a, args :: Maybe (Args a) })
varPathToPosAndArgs path = foldl step Nil path
  where
  step res (VarPathPart { name, args } _) = case name of
    VarPartNameRoot pos -> Root { pos, args } : res
    VarPartNameParent pos -> Parent { pos, args } : res
    VarPartNameGqlName gqlName pos ->
      (Pos $ Key (gqlName) { pos, args })
        : res

type PosAndArgs = { pos :: Positions, args :: Maybe (Args Positions) }

argTypeErrorToTypeError :: forall a. (List (NormalizedJsonPos a)) -> a -> (ArgTypeError a) -> TypeErrorWithPath a
argTypeErrorToTypeError pos a argTypeError = TypeErrorWithPath (ArgTypeError argTypeError) pos a
