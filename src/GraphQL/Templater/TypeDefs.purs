module GraphQL.Templater.TypeDefs
  ( GqlTypeTree(..)
  , TypeFieldValue
  , TypeMap
  , getTypeAtPath
  , getTypeMapFromTree
  , getTypeTreeFromDoc
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Array.NonEmpty as Array
import Data.Foldable (class Foldable)
import Data.GraphQL.AST (ArgumentsDefinition, OperationType(..))
import Data.GraphQL.AST as AST
import Data.Lazy (Lazy, defer, force)
import Data.Lens (Traversal', prism', toListOf, traversed)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List(..), findMap, mapMaybe)
import Data.Map (Map, lookup, unions)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Profunctor.Choice (class Choice)
import Data.String (stripSuffix)
import Data.Tuple (Tuple(..), uncurry)
import Foreign.Object as Object
import Record as Record
import Type.Proxy (Proxy(..))

type TypeMap = Map String TypeFieldValue

type TypeFieldValue = Lazy
  { args :: Maybe ArgumentsDefinition
  , returns :: GqlTypeTree
  }

data GqlTypeTree
  = Node String
  | ObjectType TypeMap
  | ListType GqlTypeTree
  | NonNull GqlTypeTree
  | GqlUndefined



getTypeAtPath :: List String -> GqlTypeTree -> Maybe GqlTypeTree
getTypeAtPath path tree = case path of
  Nil -> Just tree
  Cons p ps -> case tree of
    ObjectType m -> lookup p m <#> force >>> _.returns # maybe Nothing (getTypeAtPath ps)
    ListType t -> getTypeAtPath path t
    NonNull t -> getTypeAtPath path t
    Node n -> Just $ Node n
    GqlUndefined -> Just GqlUndefined

getTypeTreeFromDoc :: AST.Document -> Maybe GqlTypeTree
getTypeTreeFromDoc doc =
  getTypeTreeFromDefinitions
    (toListOf rootOperationTypeLens doc)
    (toListOf typeDefinitionLens doc)

getTypeTreeFromDefinitions :: List AST.RootOperationTypeDefinition -> List AST.TypeDefinition -> Maybe GqlTypeTree
getTypeTreeFromDefinitions roots defs = force <$>
  ( (queryRoot >>= flip lookup defMap)
      <|> lookup "Query" defMap
      <|> lookup "query" defMap
  )
  where
  queryRoot = roots # findMap
    ( unwrap >>> case _ of
        { operationType: Query, namedType } -> Just $ unwrap namedType
        _ -> Nothing
    )
  defMap = Map.fromFoldable $ defs <#> \def -> Tuple (tdName def) $ defer \_ -> fromAst def

  fromAst = case _ of
    AST.TypeDefinition_ScalarTypeDefinition t -> Node $ _.name $ unwrap t
    AST.TypeDefinition_ObjectTypeDefinition t -> ObjectType $ getObjectTypeMap $ unwrap t
    AST.TypeDefinition_InterfaceTypeDefinition _t -> GqlUndefined
    AST.TypeDefinition_UnionTypeDefinition t -> ObjectType $ getUnionTypeMap $ maybe Nil unwrap $ _.unionMemberTypes $ unwrap t
    AST.TypeDefinition_EnumTypeDefinition t -> Node $ _.name $ unwrap t
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
  fromNamedType (AST.NamedType nt) = case lookup name defMap of
    Just t -> force t
    Nothing -> Node name
    where 
    name = fromMaybe nt $ stripSuffix (wrap "!") nt

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
  Node _ -> Nothing
  GqlUndefined -> Nothing

typeDefinitionLens :: Traversal' AST.Document AST.TypeDefinition
typeDefinitionLens = uPrism AST._Document
  <<< traversed
  <<< uPrism AST._Definition_TypeSystemDefinition
  <<< uPrism AST._TypeSystemDefinition_TypeDefinition

rootOperationTypeLens :: Traversal' AST.Document AST.RootOperationTypeDefinition
rootOperationTypeLens = uPrism AST._Document
  <<< traversed
  <<< uPrism AST._Definition_TypeSystemDefinition
  <<< uPrism AST._TypeSystemDefinition_SchemaDefinition
  <<< _Newtype
  <<< prop (Proxy :: Proxy "rootOperationTypeDefinition")
  <<< traversed

uPrism :: forall s a c. Tuple (a -> s) (s -> Maybe a) -> (Choice c => c a a -> c s s)
uPrism = uncurry prism'
