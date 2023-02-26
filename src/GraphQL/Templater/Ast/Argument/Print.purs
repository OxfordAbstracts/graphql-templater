module GraphQL.Templater.Ast.Argument.Print where

import Prelude

import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import GraphQL.Templater.Ast.Argument (Argument(..), ListValue(..), ObjectValue(..), ArgName(..), Value(..))
import GraphQL.Templater.Ast.PrintUtils (mapWithPrevious)

printValue :: forall a. Value a -> String
printValue = case _ of
  Value_StringValue a _ -> (show $ unwrap a)
  Value_IntValue a _ -> (show $ unwrap a)
  Value_FloatValue a _ -> (show $ unwrap a)
  Value_BooleanValue a _ -> (show $ unwrap a)
  Value_NullValue _ _ -> "null"
  Value_EnumValue a _ -> (unwrap a)
  Value_Variable a _ -> ("$" <> unwrap a)
  Value_ListValue list _ -> fold
    [ "["
    , printListValues list
    , "]"
    ]
  Value_ObjectValue obj _ -> fold
    [ "{"
    , printObjectValues obj
    , "}"
    ]
  where
  printListValues :: ListValue a -> String
  printListValues (ListValue vals) = fold $ mapWithPrevious printListValue vals

  printListValue :: Maybe (Value a) -> Value a -> String
  printListValue prev val = case prev of
    Just _v -> fold [ ", ", printValue val ]
    _ -> printValue val

printObjectValues :: forall a. ObjectValue a -> String
printObjectValues (ObjectValue vals) = fold $ mapWithPrevious printObjectValue vals

printObjectValue :: forall a. Maybe (Argument a) -> (Argument a) -> String
printObjectValue prev (Argument { name, value }) = case prev of
  Just (Argument _) -> fold [ ", ", printObjectValue' name value ]
  _ -> printObjectValue' name value

printObjectValue' :: forall a. ArgName a -> Value a -> String
printObjectValue' (ArgName name _) value = fold
  [ name
  , ": "
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