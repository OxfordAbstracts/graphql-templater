module GraphQL.Templater.TypeCheck.Errors.Display where

import Prelude

import Data.Array as Array
import Data.GraphQL.AST.Print (printAst)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set as Set
import GraphQL.Templater.JsonPos (NormalizedJsonPos(..))
import GraphQL.Templater.LevenshteinDistance (closest)
import GraphQL.Templater.TypeCheck.Errors (ArgTypeError(..), MismatchReason(..), TypeError(..), TypeErrorWithPath(..))

displayPositionedError :: forall a. TypeErrorWithPath a -> String
displayPositionedError (TypeErrorWithPath typeError path _pos) = case typeError of
  FieldNotFound fields ->
    msg <> suggestion
    where
    msg = case nodeField, parentField of
      Just node, Just parent -> "Field " <> show node <> " not found on " <> show parent <> "."
      Just node, _ -> "Field " <> show node <> " not found."
      _, _ -> "Not found"

    suggestion = fromMaybe "" do
      node <- nodeField
      close <- closest node $ Array.fromFoldable fields
      pure $ "\nPerhaps you meant " <> show close <> "?"

  NotObject -> fromMaybe "Not an object" do
    parent <- parentField
    pure $ show parent <> " is not an object.\n You cannot perform a dot lookup on it."

  ObjectWhenNodeExpected fields -> case nodeField of
    Just node -> show node <> " is an object, not a node." <> suggestion
      where
      suggestion = Set.findMin fields # maybe "" \f ->
        "\nPerhaps you meant. " <> show (node <> "." <> f)
    _ -> "Object found, not node"

  NotList -> "Not a list"

  ArgTypeError err -> displayArgTypeError err

  where
  nodeField :: Maybe String
  nodeField = do
    idx <- nodeIdx reversed
    getKeyAt idx

  parentField :: Maybe String
  parentField = do
    idx <- nodeIdx reversed
    reversed
      # List.drop (idx + 1)
      # List.findMap getKey

  nodeIdx = List.findIndex (getKey >>> isJust)

  getKey = case _ of
    Key key _ -> Just key
    _ -> Nothing

  getKeyAt idx = List.index reversed idx >>= getKey

  reversed = List.reverse path

displayArgTypeError :: forall a. ArgTypeError a -> String
displayArgTypeError = case _ of
  ArgRequired field -> show field <> " is a required argument"
  ArgUnknown field _ -> show field <> " is not a valid argument"
  ArgTypeMismatch { name, argValue, definitionType, reasons } _ ->
    case NonEmptyList.head reasons of
      ValueDoesNotFitDefinition ->
        "For field " <> show name <> ", the value of " <> printAst argValue
          <> ", does not fit the definition of "
          <> printAst definitionType

      NullArgForNonNullType ->
        "For field " <> show name
          <> ", the value of is null, but the definition of "
          <> printAst definitionType
          <> " is non-null"

