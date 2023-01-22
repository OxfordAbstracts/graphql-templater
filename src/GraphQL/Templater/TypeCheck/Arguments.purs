module GraphQL.Templater.TypeCheck.Arguments where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl)
import Data.GraphQL.AST (ArgumentsDefinition, InputValueDefinition(..))
import Data.GraphQL.AST as AST
import Data.List (List(..), (:))
import Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Debug (spy, spyWith)
import GraphQL.Templater.Ast (Arg(..), ArgName(..), Args, Value(..))
import GraphQL.Templater.TypeCheck.Errors (ArgTypeError(..), MismatchReason(..))

typeCheckArguments :: forall a. Maybe ArgumentsDefinition -> Maybe (Args a) -> List (ArgTypeError a)
typeCheckArguments argsDef = spyWith "typeCheckArguments" Array.fromFoldable <<< go (maybe Nil unwrap argsDef) <<< fromMaybe Nil
  where
  go :: List InputValueDefinition -> Args a -> List (ArgTypeError a)
  go defs args = foldl checkDef Nil defs <> foldl checkArg Nil (spyWith "args" Array.fromFoldable args)
    where
    checkDef :: List (ArgTypeError a) -> InputValueDefinition -> List (ArgTypeError a)
    checkDef res (InputValueDefinition { defaultValue, name, type: type_ }) =
      case Map.lookup name argsMap of
        Nothing
          | isJust defaultValue || isNullableType type_ -> res
          | true -> ArgRequired name : res

        Just (Arg { value: Value value a } _)
          | Just reasons <- NonEmpty.fromList $ valueIsOfType type_ value ->
              ArgTypeMismatch
                { name
                , definitionType: type_
                , argValue: value
                , reasons
                }
                a :
                res

        _ -> res

    isNullableType :: AST.Type -> Boolean
    isNullableType = case _ of
      AST.Type_NamedType _ -> true
      AST.Type_ListType _ -> true
      AST.Type_NonNullType _ -> false

    checkArg :: List (ArgTypeError a) -> Arg a -> List (ArgTypeError a)
    checkArg res (Arg { name: ArgName name a } _) = spyWith ("checkArg " <> name) Array.fromFoldable
      case Map.lookup name defsMap of
        Nothing -> ArgUnknown name a : res
        _ -> res

    valueIsOfType :: AST.Type -> AST.Value -> List MismatchReason
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
      checkMismatch name = case _ of
        Just valName | name /= valName -> pure ValueDoesNotFitDefinition
        _ -> Nil

      isNotNull :: AST.Value -> List MismatchReason
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
        AST.Value_ObjectValue _ -> Nothing -- TODO: add support for objects
        AST.Value_NullValue _ -> Nothing
        AST.Value_StringValue _ -> Just "String"
        AST.Value_IntValue _ -> Just "Int"
        AST.Value_FloatValue _ -> Just "Float"
        AST.Value_BooleanValue _ -> Just "Boolean"
        AST.Value_EnumValue _ -> Just "String"

    defsMap :: Map String InputValueDefinition
    defsMap = Map.fromFoldable $ map (\def@(InputValueDefinition { name }) -> Tuple name def) defs

    argsMap :: Map String (Arg a)
    argsMap = Map.fromFoldable $ map (\arg@(Arg { name } _) -> Tuple (argNameVal name) arg) args

argNameVal :: forall a. ArgName a -> String
argNameVal (ArgName name _) = name