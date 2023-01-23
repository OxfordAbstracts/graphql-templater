module Test.GraphQL.Templater.Eval.MakeQuery
  ( spec
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (global)
import Effect.Exception (Error)
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Eval.MakeQuery as MakeQuery
import GraphQL.Templater.Parser (parse)
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

      it "should return a query with only __typenames for an each without variables" do
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
          ) `shouldEqualWoWs` Just "query {  __typename test {  __typename } } "

      it "should return a graphql query for a simple variable ast" do
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
          ) `shouldEqualWoWs` Just "query { __typename test } "

      it "should return a graphql query for a variable in an each" do
        parseAndMakeQuery "{{#each item}}{{field}}{{/each}}"
          `shouldEqualWoWs` Just "query { __typename item { __typename field } } "

      it "should return a graphql query for a variable in a with" do
        parseAndMakeQuery "{{#with item}}{{field}}{{/with}}"
          `shouldEqualWoWs` Just "query { __typename item { __typename field } } "

shouldEqualWoWs :: forall m. MonadThrow Error m => Maybe String -> Maybe String -> m Unit
shouldEqualWoWs a b = map noWhitespace a `shouldEqual` map noWhitespace b
  where
  noWhitespace = replace rgx " "

  rgx = case regex """\s+""" global of
    Left err -> unsafeCrashWith $ "regex error: " <> show err
    Right r -> r

parseAndMakeQuery :: String -> Maybe String
parseAndMakeQuery str = case parse str of
  Left err -> unsafeCrashWith $ "parse error: " <> show err
  Right ast -> toGqlString $ map (const unit) <$> ast

toGqlString :: List (Ast Unit) -> Maybe String
toGqlString = MakeQuery.toGqlString