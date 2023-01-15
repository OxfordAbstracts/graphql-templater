module GraphQL.Templater.TypeCheck.Arguments where

import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.GraphQL.AST (Argument(..), Arguments, ArgumentsDefinition, InputValueDefinition(..), Value)
import Data.GraphQL.AST as AST
import Data.List (List(..), (:))
import Data.List.NonEmpty as NonEmpty
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))

data ArgTypeError
  = ArgUnknown String
  | ArgRequired String
  | ArgTypeMismatch
      { name :: String
      , definitionType :: AST.Type
      , argValue :: AST.Value
      , reasons :: NonEmptyList MismatchReason
      }

derive instance Generic ArgTypeError _
derive instance Eq ArgTypeError
instance Show ArgTypeError where
  show = genericShow

data MismatchReason = NullArgForNonNullType | InvalidType

derive instance Generic MismatchReason _
derive instance Eq MismatchReason
instance Show MismatchReason where
  show = genericShow

typeCheckArguments :: Maybe ArgumentsDefinition -> Maybe Arguments -> List ArgTypeError
typeCheckArguments argsDef = go (maybe Nil unwrap argsDef) <<< maybe Nil unwrap
  where
  go :: List InputValueDefinition -> List Argument -> List ArgTypeError
  go defs args = foldl checkDef Nil defs <> foldl checkArg Nil args
    where
    checkDef :: List ArgTypeError -> InputValueDefinition -> List ArgTypeError
    checkDef res (InputValueDefinition { defaultValue, name, type: type_ }) =
      case Map.lookup name argsMap of
        Nothing
          | isJust defaultValue || isNullableType type_ -> res
          | true -> ArgRequired name : res

        Just (Argument { value })
          | Just reasons <- NonEmpty.fromList $ valueIsOfType type_ value ->
              ArgTypeMismatch
                { name
                , definitionType: type_
                , argValue: value
                , reasons
                } :
                res

        _ -> res

    isNullableType :: AST.Type -> Boolean
    isNullableType = case _ of
      AST.Type_NamedType _ -> true
      AST.Type_ListType _ -> true
      AST.Type_NonNullType _ -> false

    checkArg :: List ArgTypeError -> Argument -> List ArgTypeError
    checkArg res (Argument { name }) =
      case Map.lookup name defsMap of
        Nothing -> ArgUnknown name : res
        _ -> res

    valueIsOfType :: AST.Type -> Value -> List MismatchReason
    valueIsOfType type_ value = case type_ of
      AST.Type_NamedType (AST.NamedType name) ->
        isOfType name valueName
      AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType name)) ->
        isNotNull value <> isOfType name valueName
      AST.Type_NonNullType (AST.NonNullType_ListType (AST.ListType t')) ->
        isNotNull value <> handleListType t'
      AST.Type_ListType (AST.ListType t') ->
        handleListType t'
      where
      isOfType :: String -> Maybe String -> List MismatchReason
      isOfType name valName = case name of
        "ID" -> case checkMismatch "String" valName of
          Nil -> Nil
          _ -> checkMismatch "Int" valName
        _ -> checkMismatch name valName

      checkMismatch :: String -> Maybe String -> List MismatchReason
      checkMismatch _ _ = Nil

      isNotNull :: Value -> List MismatchReason
      isNotNull = case _ of
        AST.Value_NullValue _ -> pure NullArgForNonNullType
        _ -> Nil

      handleListType t = case value of
        AST.Value_ListValue (AST.ListValue values) ->
          foldl (\res v -> valueIsOfType t v <> res) Nil values
        _ -> Nil

      valueName = case value of
        AST.Value_Variable _ -> Nothing -- TODO: add support for variables
        AST.Value_ListValue _ -> Nothing -- TODO: add support for list
        AST.Value_ObjectValue _ -> Nothing -- TODO: add support for variables
        AST.Value_NullValue _ -> Nothing
        AST.Value_StringValue _ -> Just "String"
        AST.Value_IntValue _ -> Just "Int"
        AST.Value_FloatValue _ -> Just "Float"
        AST.Value_BooleanValue _ -> Just "Boolean"
        AST.Value_EnumValue _ -> Just "String"

    defsMap :: Map String InputValueDefinition
    defsMap = Map.fromFoldable $ map (\def@(InputValueDefinition { name }) -> Tuple name def) defs

    argsMap :: Map String Argument
    argsMap = Map.fromFoldable $ map (\arg@(Argument { name }) -> Tuple name arg) args

