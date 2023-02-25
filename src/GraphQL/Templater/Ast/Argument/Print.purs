module GraphQL.Templater.Ast.Argument.Print where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import GraphQL.Templater.Ast.Argument (Argument(..), ListValue(..), ObjectValue(..), ArgName(..), Value(..))
import GraphQL.Templater.Ast.PrintUtils (atEnd, atStart, combine, mapWithPrevious)
import GraphQL.Templater.Positions (Positions)

printValue :: forall a. Value a -> String
printValue = case _ of
  Value_StringValue a _ -> atStart unit (show $ unwrap a)
  Value_IntValue a _ -> atStart unit (show $ unwrap a)
  Value_FloatValue a _ -> atStart unit (show $ unwrap a)
  Value_BooleanValue a _ -> atStart unit (show $ unwrap a)
  Value_NullValue _ _ -> atStart unit "null"
  Value_EnumValue a _ -> atStart unit (unwrap a)
  Value_Variable a _ -> atStart unit ("$" <> unwrap a)
  Value_ListValue list _ -> combine
    [ atStart unit "["
    , printListValues list
    , atEnd unit "]"
    ]
  Value_ObjectValue obj _ -> combine
    [ atStart unit "{"
    , printObjectValues obj
    , atEnd unit "}"
    ]
  where
  printListValues :: ListValue a -> String
  printListValues (ListValue vals) = combine $ mapWithPrevious printListValue vals

  printListValue :: Maybe (Value a) -> Value a -> String
  printListValue prev val = case prev of
    Just v -> combine [ atStart unit ", ", printValue val ]
    _ -> printValue val

printObjectValues :: forall a. ObjectValue a -> String
printObjectValues (ObjectValue vals) = combine $ mapWithPrevious printObjectValue vals

printObjectValue :: forall a. Maybe (Argument a) -> (Argument a) -> String
printObjectValue prev (Argument { name, value }) = case prev of
  Just (Argument prevArg) -> combine [ atStart unit ", ", printObjectValue' name value ]
  _ -> printObjectValue' name value

printObjectValue' :: forall a. ArgName a -> Value a -> String
printObjectValue' (ArgName name strPos) value = combine
  [ atStart unit name
  , atStart unit ": "
  , printValue value
  ]

getValuePos :: forall a. Value a -> a
getValuePos = case _ of
  Value_StringValue _ a -> a
  Value_IntValue _ a -> a
  Value_FloatValue _ a -> a
  Value_BooleanValue _ a -> a
  Value_NullValue _ a -> a
  Value_EnumValue _ a -> a
  Value_Variable _ a -> a
  Value_ListValue _ a -> a
  Value_ObjectValue _ a -> a