module Test.GraphQL.Templater.Eval.MakeQuery
  ( spec
  )
  where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Effect.Exception (Error)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Eval.MakeQuery (toGqlString)
import Partial.Unsafe (unsafeCrashWith)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Eval.MakeQuery" do
    describe "toGqlString" do
      it "should return Nothing for empty asts" do
        toGqlString Nil `shouldEqual` Nothing
        toGqlString (Text "" unit : Nil) `shouldEqual` Nothing

      it "should return Nothing for an ast without variables" do
        toGqlString
          ( Each
              ( VarPath
                  ( pure $ VarPathPart
                      { args: Nothing
                      , name: VarPartNameGqlName "test" unit
                      }
                      unit
                  )
                  unit
              )
              ( Text "text" unit : Nil
              )
              unit : Nil
          ) `shouldEqual` Nothing

      it "should return a graphql query when for a simple variable ast" do
        toGqlString
          ( Var
              ( VarPath
                  ( pure $ VarPathPart
                      { args: Nothing
                      , name: VarPartNameGqlName "test" unit
                      }
                      unit
                  )
                  unit
              )
              unit : Nil
          ) `shouldEqualWoWs` Just "query { test } "

      it "should return a graphql query when for a variable in an each" do
        toGqlString
          ( Each
              ( VarPath
                  ( pure $ VarPathPart
                      { args: Nothing
                      , name: VarPartNameGqlName "item" unit
                      }
                      unit
                  )
                  unit
              )
              ( Var
                  ( VarPath
                      ( pure $ VarPathPart
                          { args: Nothing
                          , name: VarPartNameGqlName "field" unit
                          }
                          unit
                      )
                      unit
                  )
                  unit : Nil
              )
              unit : Nil
          ) `shouldEqualWoWs` Just "query {item { field } }"


shouldEqualWoWs :: forall m. MonadThrow Error m => Maybe String -> Maybe String -> m Unit
shouldEqualWoWs a b = map noWhitespace a `shouldEqual` map noWhitespace b
  where 
  noWhitespace = replace rgx ""

  rgx = case regex """\s+""" global of
    Left err -> unsafeCrashWith $ "regex error: " <> show err
    Right r -> r

