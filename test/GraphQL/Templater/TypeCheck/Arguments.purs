module Test.GraphQL.Templater.TypeCheck.Arguments (spec) where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Templater.Ast (Arg(..), ArgName(..), Value(..))
import GraphQL.Templater.TypeCheck.Arguments (typeCheckArguments)
import GraphQL.Templater.TypeCheck.Errors (ArgTypeError(..), MismatchReason(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.TypeDefs.Arguments" do
    describe "getTypeErrorsFromTree" do
      it "should return no errors when there is no args and no definitions" do
        typeCheck Nil Nil `shouldEqual` Nil

      it "should return no errors when there is one matching arg and definition" do
        typeCheck (defaultDef : Nil) (defaultArg : Nil) `shouldEqual` Nil

      it "should return no errors when a string or int is provided for an ID" do
        typeCheck
          ( defaultDef
              { type = AST.Type_NamedType $ AST.NamedType "ID"
              }
              : Nil
          )
          (defaultArg : Nil)
          `shouldEqual` Nil

        typeCheck
          ( defaultDef
              { type = AST.Type_NamedType $ AST.NamedType "ID"
              }
              : Nil
          )
          ( defaultArg
              { value = AST.Value_IntValue $ AST.IntValue 1
              }
              : Nil
          )
          `shouldEqual` Nil
      it "should return no errors when a string or int is provided for a non null ID" do
        typeCheck
          ( defaultDef
              { type = AST.Type_NonNullType $ AST.NonNullType_NamedType $ AST.NamedType "ID"
              }
              : Nil
          )
          (defaultArg : Nil)
          `shouldEqual` Nil

        typeCheck
          ( defaultDef
              { type = AST.Type_NonNullType $ AST.NonNullType_NamedType $ AST.NamedType "ID"
              }
              : Nil
          )
          ( defaultArg
              { value = AST.Value_IntValue $ AST.IntValue 1
              }
              : Nil
          )
          `shouldEqual` Nil

      it "should return an ArgRequired error when there is no matching arg for a non null definition without a default value" do
        typeCheck
          ( defaultDef
              { type = AST.Type_NonNullType $ AST.NonNullType_NamedType $ AST.NamedType "String"
              }
              : Nil
          )
          Nil `shouldEqual` pure (ArgRequired "default")

      it "should return no errors when there is no matching arg for a non null definition with a default value" do
        typeCheck
          ( defaultDef
              { type = AST.Type_NonNullType $ AST.NonNullType_NamedType $ AST.NamedType "String"
              , defaultValue = Just $ AST.DefaultValue $ AST.Value_StringValue $ AST.StringValue "test"
              }
              : Nil
          )
          Nil `shouldEqual` Nil

      it "should return an ArgUnknown error when an arg is not a parameter" do
        typeCheck
          Nil
          (defaultArg : Nil)
          `shouldEqual`
            pure (ArgUnknown "default" unit)

      it "should return an ArgTypeMismatch error when an arg is of the wrong type" do

        let
          def = defaultDef
            { type = AST.Type_NamedType $ AST.NamedType "Int"
            }
        typeCheck
          ( def : Nil
          )
          ( defaultArg
              : Nil
          )
          `shouldEqual`
            pure
              ( ArgTypeMismatch
                  { name: "default"
                  , definitionType: def.type
                  , argValue: defaultArg.value
                  , reasons: pure ValueDoesNotFitDefinition
                  }
                  unit
              )

defaultDef :: AST.T_InputValueDefinition
defaultDef =
  { name: "default"
  , type: AST.Type_NamedType $ AST.NamedType "String"
  , defaultValue: Nothing
  , description: Nothing
  , directives: Nothing
  }

defaultArg :: AST.T_Argument
defaultArg =
  { name: "default"
  , value: AST.Value_StringValue $ AST.StringValue "default"
  }

typeCheck :: List AST.T_InputValueDefinition -> List AST.T_Argument -> List (ArgTypeError Unit)
typeCheck defs args =
  typeCheckArguments
    (Just $ AST.ArgumentsDefinition $ map AST.InputValueDefinition defs)
    (Just $ map toArg args)
  where
  toArg :: AST.T_Argument -> Arg Unit
  toArg { name, value } = Arg { name: ArgName name unit, value: Value value unit } unit