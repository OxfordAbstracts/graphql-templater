module GraphQL.Templater.Ast.Argument.ToGqlValue where

import Prelude

import Data.GraphQL.AST as AST
import Data.Newtype (unwrap, wrap)
import GraphQL.Templater.Ast.Argument (ArgName(..))
import GraphQL.Templater.Ast.Argument as Template

toGqlValue :: forall a. Template.Value a -> AST.Value
toGqlValue = case _ of
  Template.Value_Variable name _ -> AST.Value_Variable $ wrap $ unwrap name
  Template.Value_IntValue int _ -> AST.Value_IntValue $ wrap $ unwrap int
  Template.Value_FloatValue float _ -> AST.Value_FloatValue $ wrap $ unwrap float
  Template.Value_StringValue string _ -> AST.Value_StringValue $ wrap $ unwrap string
  Template.Value_BooleanValue bool _ -> AST.Value_BooleanValue $ wrap $ unwrap bool
  Template.Value_EnumValue enum _ -> AST.Value_EnumValue $ wrap $ unwrap enum
  Template.Value_NullValue _ _ -> AST.Value_NullValue AST.NullValue
  Template.Value_ListValue list _ -> AST.Value_ListValue (wrap $ map toGqlValue $ unwrap list)
  Template.Value_ObjectValue object _ -> AST.Value_ObjectValue (wrap $ map toGqlArg $ unwrap object)

  where
  toGqlArg :: Template.Argument a -> AST.Argument
  toGqlArg (Template.Argument { name: ArgName name _, value }) = AST.Argument
    { name
    , value: toGqlValue value
    }

fromGqlValue :: AST.Value -> Template.Value Unit
fromGqlValue = case _ of
  AST.Value_Variable (AST.Variable name) -> Template.Value_Variable (wrap name) unit
  AST.Value_IntValue (AST.IntValue int) -> Template.Value_IntValue (wrap int) unit
  AST.Value_FloatValue (AST.FloatValue float) -> Template.Value_FloatValue (wrap float) unit
  AST.Value_StringValue (AST.StringValue string) -> Template.Value_StringValue (wrap string) unit
  AST.Value_BooleanValue (AST.BooleanValue bool) -> Template.Value_BooleanValue (wrap bool) unit
  AST.Value_EnumValue (AST.EnumValue enum) -> Template.Value_EnumValue (wrap enum) unit
  AST.Value_NullValue AST.NullValue -> Template.Value_NullValue Template.NullValue unit
  AST.Value_ListValue (AST.ListValue list) -> Template.Value_ListValue (wrap $ map fromGqlValue list) unit
  AST.Value_ObjectValue (AST.ObjectValue object) -> Template.Value_ObjectValue (wrap $ map fromGqlArg object) unit

  where
  fromGqlArg :: AST.Argument -> Template.Argument _
  fromGqlArg (AST.Argument { name, value }) = Template.Argument
    { name: ArgName name unit
    , value: fromGqlValue value
    , pos: unit
    }