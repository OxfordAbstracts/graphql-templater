module GraphQL.Templater.TypeCheck.Errors.Display where

import Prelude

import Data.List as List
import Data.Maybe (Maybe(..), isJust)
import GraphQL.Templater.JsonPos (NormalizedJsonPos(..))
import GraphQL.Templater.Positions (Positions)
import GraphQL.Templater.TypeCheck.Errors (ArgTypeError(..), PositionedError, TypeError(..), TypeErrorWithPath(..))

displayPositionedError :: forall a. TypeErrorWithPath a -> String
displayPositionedError (TypeErrorWithPath typeError path _pos) = case typeError of
  FieldNotFound -> case nodeField, parentField of 
    Just node, Just parent -> "Field " <> show node <> " not found on " <> show parent
    Just node, _ -> "Field " <> show node <> " not found"
    _, _ -> "Not found"

  NotObject -> "Not an object"

  NotNode -> "Not a node field"

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

  parent = List.head =<< List.tail reversed

  reversed = List.reverse path


displayArgTypeError :: forall a. ArgTypeError a -> String 
displayArgTypeError = case _ of 
  ArgRequired field -> show field <> " is a required argument"
  ArgUnknown field _ -> show field <> " is not a valid argument"
  ArgTypeMismatch {name, argValue, definitionType, reasons} _ -> ""