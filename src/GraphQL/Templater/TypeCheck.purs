module GraphQL.Templater.TypeCheck where

import Prelude

import Control.Monad.State (execState, modify_)
import Data.Either (Either(..), either)
import Data.Foldable (foldM, foldMap)
import Data.GraphQL.AST (ArgumentsDefinition(..))
import Data.Lazy (force)
import Data.List (List(..), head, (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPath(..), VarPathPart(..))
import GraphQL.Templater.JsonPos (JsonPos, NormalizedJsonPos(..), normalizePos, varPathToPosition)
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), TypeMap, getTypeMapFromTree)
import Unsafe.Coerce (unsafeCoerce)

data TypeError
  = FieldNotFound String
  | FieldNotObject String
  | FieldNotNode String
  | FieldNotList String

type PositionedError = { error :: TypeError, positions :: Positions }

getTypeErrors :: TypeMap -> List AstPos -> List PositionedError
getTypeErrors types asts' = _.errors $ execState (goAsts asts') initialState
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
    case head asts of
      Nothing -> pure unit
      Just ast -> do
        case ast of
          Var (VarPath v _) p -> modify_ \st -> st
            { errors = getPathErrors types (normalizePos $ varPathToPosition v <> st.path)
                <> st.errors
            }
          Each v inner p -> pure unit
          Text _ _ -> pure unit

  getPathErrors
    :: TypeMap
    -> List (NormalizedJsonPos Positions)
    -> List PositionedError
  getPathErrors types' = case _ of
    Nil -> Nil

    Cons (Key k p) rest@(Cons _ _) ->
      lookupType types' k p
        # either pure \{ args, returns } -> go returns
      where
      go = case _ of
        ObjectType obj -> getPathErrors obj rest
        NonNull t -> go t
        ListType _ -> pure { error: FieldNotObject k, positions: p }
        Node -> pure { error: FieldNotObject k, positions: p }
        GqlUndefined -> pure { error: FieldNotObject k, positions: p }

    Cons (Key k p) Nil ->
      lookupType types' k p
        # either pure \{ args, returns } -> go returns
      where
      go =
        case _ of
          Node -> Nil
          NonNull t -> go t
          _ -> pure { error: FieldNotNode k, positions: p }

    Cons (Index i p) rest@(Cons _ _) -> Nil

    Cons (Index i p) Nil -> Nil
    -- lookupType types' p 
    --   # either pure \{ args, returns } -> Nil
    -- case returns of
    --   ListType t -> case t of 
    --   _ -> pure { error: FieldNotList k, positions: p }

    where
    lookupType
      :: TypeMap
      -> String
      -> Positions
      -> Either PositionedError
           { args :: Maybe ArgumentsDefinition
           , returns :: GqlTypeTree
           }
    lookupType t k p = case force <$> Map.lookup k t of
      Nothing -> Left { error: FieldNotFound k, positions: p }
      Just ret -> Right ret
