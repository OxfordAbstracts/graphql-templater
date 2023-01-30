module GraphQL.Templater.Ast.Argument.Print where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import GraphQL.Templater.Ast.Argument (Argument(..), ListValue(..), ObjectValue(..), StringWith(..), Value(..))
import GraphQL.Templater.Ast.PrintUtils (class PrintKey, PrintResult, atStart, combine, mapWithPrevious)
import GraphQL.Templater.Positions (Positions)

printValue :: forall k. PrintKey k => Value Positions -> PrintResult k
printValue = case _ of
  Value_StringValue a { start } -> atStart start (show $ unwrap a)
  Value_IntValue a { start } -> atStart start (show $ unwrap a)
  Value_FloatValue a { start } -> atStart start (show $ unwrap a)
  Value_BooleanValue a { start } -> atStart start (show $ unwrap a)
  Value_NullValue _ { start } -> atStart start "null"
  Value_EnumValue a { start } -> atStart start (unwrap a)
  Value_Variable a { start } -> atStart start ("$" <> unwrap a)
  Value_ListValue list { start, end } -> combine
    [ atStart start "["
    , printListValues list
    , atStart end "]"
    ]
  Value_ObjectValue obj { start, end } -> combine
    [ atStart start "{"
    , printObjectValues obj
    , atStart end "}"
    ]
  where
  printListValues :: ListValue Positions -> PrintResult k
  printListValues (ListValue vals) = combine $ mapWithPrevious printListValue vals

  printListValue :: Maybe (Value Positions) -> Value Positions -> PrintResult k
  printListValue prev val = case prev of
    Just v -> combine [ atStart (getValuePos v).end ", ", printValue val ]
    _ -> printValue val

printObjectValues :: forall k. PrintKey k => ObjectValue Positions -> PrintResult k
printObjectValues (ObjectValue vals) = combine $ mapWithPrevious printObjectValue vals

printObjectValue :: forall k. PrintKey k => Maybe (Argument Positions) -> (Argument Positions) -> PrintResult k
printObjectValue prev (Argument { name, value }) = case prev of
  Just (Argument prevArg) -> combine [ atStart prevArg.pos.end ", ", printObjectValue' name value ]
  _ -> printObjectValue' name value

printObjectValue' :: forall k. PrintKey k => StringWith Positions -> Value Positions -> PrintResult k
printObjectValue' (StringWith name strPos) value = combine
  [ atStart strPos.start name
  , atStart strPos.end ": "
  , printValue value
  ]

getValuePos :: Value Positions -> Positions
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