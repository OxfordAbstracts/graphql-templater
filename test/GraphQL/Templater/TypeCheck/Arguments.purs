module Test.GraphQL.Templater.TypeCheck.Arguments (spec) where

import Prelude

import Data.GraphQL.AST as AST
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Templater.TypeCheck.Arguments (ArgTypeError(..), typeCheckArguments)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.TypeDefs" do
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

typeCheck :: List AST.T_InputValueDefinition -> List AST.T_Argument -> List ArgTypeError
typeCheck defs args =
  typeCheckArguments
    (Just $ AST.ArgumentsDefinition $ map AST.InputValueDefinition defs)
    (Just $ AST.Arguments $ map AST.Argument args)