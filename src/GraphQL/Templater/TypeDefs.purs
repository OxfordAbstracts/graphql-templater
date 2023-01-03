module GraphQL.Templater.TypeDefs
  ( GqlTypeTree(..)
  , TypeFieldValue
  , TypeMap
  , getTypeMap
  , getTypeMapFromTree
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.GraphQL.AST (ArgumentsDefinition(..))
import Data.GraphQL.AST as AST
import Data.Lazy (Lazy, defer, force)
import Data.List (List(..), mapMaybe)
import Data.Map (Map, lookup, unions)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Record as Record

type TypeMap = Map String TypeFieldValue

type TypeFieldValue = Lazy
  { args :: Maybe ArgumentsDefinition
  , returns :: GqlTypeTree
  }

data GqlTypeTree
  = Node
  | ObjectType TypeMap
  | ListType GqlTypeTree
  | NonNull GqlTypeTree
  | GqlUndefined

getTypeMap :: List AST.TypeDefinition -> TypeMap
getTypeMap defs = case force <$> (lookup "Query" defMap <|> lookup "query" defMap) of
  Just (ObjectType query) -> query
  _ -> Map.empty
  where
  defMap = Map.fromFoldable $ defs <#> \def -> Tuple (tdName def) $ defer \_ -> fromAst def

  fromAst = case _ of
    AST.TypeDefinition_ScalarTypeDefinition _t -> Node
    AST.TypeDefinition_ObjectTypeDefinition t -> ObjectType $ getObjectTypeMap $ unwrap t
    AST.TypeDefinition_InterfaceTypeDefinition _t -> GqlUndefined
    AST.TypeDefinition_UnionTypeDefinition t -> ObjectType $ getUnionTypeMap $ maybe Nil unwrap $ _.unionMemberTypes $ unwrap t
    AST.TypeDefinition_EnumTypeDefinition _t -> Node
    AST.TypeDefinition_InputObjectTypeDefinition t -> ObjectType $ getInputObjectTypeMap $ unwrap t

  getObjectTypeMap
    :: forall r
     . { fieldsDefinition :: Maybe AST.FieldsDefinition
       , implementsInterfaces :: Maybe AST.ImplementsInterfaces
       , name :: String
       | r
       }
    -> TypeMap
  getObjectTypeMap { fieldsDefinition, implementsInterfaces } = Map.union fieldsMap implementsMaps
    where
    fieldsMap = Map.fromFoldable $ maybe Nil unwrap fieldsDefinition <#> unwrap >>> fieldToTreePair

    implementsMaps = getUnionTypeMap $ maybe Nil unwrap implementsInterfaces

  getInputObjectTypeMap
    :: forall r
     . { inputFieldsDefinition :: Maybe AST.InputFieldsDefinition
       , name :: String
       | r
       }
    -> TypeMap
  getInputObjectTypeMap { inputFieldsDefinition } =
    Map.fromFoldable $ maybe Nil unwrap inputFieldsDefinition <#> unwrap
      >>> Record.merge { argumentsDefinition: Nothing }
      >>> fieldToTreePair

  getUnionTypeMap :: List AST.NamedType -> TypeMap
  getUnionTypeMap types = unions $ mapMaybe (getTypeMapFromTree <<< fromNamedType) types

  fieldToTreePair
    :: forall r
     . { name :: String
       , argumentsDefinition :: Maybe ArgumentsDefinition
       , type :: AST.Type
       | r
       }
    -> Tuple String TypeFieldValue
  fieldToTreePair { argumentsDefinition, name, type: tipe } =
    Tuple name $ defer \_ -> { args: argumentsDefinition, returns: fromAstType tipe }

  fromAstType :: AST.Type -> GqlTypeTree
  fromAstType = case _ of
    AST.Type_NamedType t -> fromNamedType t
    AST.Type_ListType (AST.ListType t) -> ListType $ fromAstType t
    AST.Type_NonNullType t -> NonNull $ fromNonNullType t

  fromNonNullType :: AST.NonNullType -> GqlTypeTree
  fromNonNullType = case _ of
    AST.NonNullType_NamedType t -> fromNamedType t
    AST.NonNullType_ListType (AST.ListType t) -> ListType $ fromAstType t

  fromNamedType :: AST.NamedType -> GqlTypeTree
  fromNamedType (AST.NamedType nt) = case lookup nt defMap of
    Just t -> force t
    Nothing -> GqlUndefined

  tdName :: AST.TypeDefinition -> String
  tdName = case _ of
    AST.TypeDefinition_ObjectTypeDefinition t -> _.name $ unwrap t
    AST.TypeDefinition_ScalarTypeDefinition t -> _.name $ unwrap t
    AST.TypeDefinition_InterfaceTypeDefinition t -> _.name $ unwrap t
    AST.TypeDefinition_UnionTypeDefinition t -> _.name $ unwrap t
    AST.TypeDefinition_EnumTypeDefinition t -> _.name $ unwrap t
    AST.TypeDefinition_InputObjectTypeDefinition t -> _.name $ unwrap t

getTypeMapFromTree :: GqlTypeTree -> Maybe TypeMap
getTypeMapFromTree = case _ of
  ObjectType m -> Just m
  ListType t -> getTypeMapFromTree t
  NonNull t -> getTypeMapFromTree t
  Node -> Nothing
  GqlUndefined -> Nothing