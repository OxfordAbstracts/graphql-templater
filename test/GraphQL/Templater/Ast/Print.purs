module Test.GraphQL.Templater.Ast.Print where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
-- import Data.Array.NonEmpty (any)
import Data.Either (Either(..))
import Data.List (List, all, any)
import Data.String (trim)
import Effect.Exception (Error, error)
import GraphQL.Templater.Ast (Ast)
import GraphQL.Templater.Ast as Ast
import GraphQL.Templater.Ast.Print (printTemplateAsts)
import GraphQL.Templater.Parser (parse)
import GraphQL.Templater.Positions (Positions)
import Parsing (parseErrorMessage)
import Test.Spec (Spec, SpecT, describe, it)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Ast.Print" do
    describe "printTemplateAsts" do
      roundTrip "text only"

      roundTrip " text with whitespace "

      roundTrip "\n text with newlines \n"

      roundTrip " \n text with newlines and whitespace \n "

      roundTrip "var {{variable}}"

      roundTrip "chained {{chained.variable}}"

      roundTrip "parents and roots {{chained.*parent.*root}}"

      roundTrip "arg {{variable(arg1: 1)}}"

      roundTrip "args {{variable(arg1: 1, arg2: 2)}}"

      roundTrip "args on args {{chained(b: true).variable(arg1: 1, arg2: 2)}}"

      roundTripNamed [ hasEach ] "eaches" """{{#each array }}text{{/each}}"""

      roundTripNamed [ hasEach ] "eaches on newlines"
        """{{#each array }} 
      text{{/each}} """

      roundTripNamed [ hasEach ] "eaches with variables"
        """{{#each array }}
      {{var.a}} {{/each}}"""

      roundTripNamed [ hasWith ] "withs with variables with args"
        """{{#with obj }}
      {{var.a(a: "b")}} {{/with}}"""

      roundTripNamed [ hasEach, hasWith ] "a complex example"
        """{{#each items}}
      {{#each inner-items}}{{var.a(a: "b")}}{{/each}} some text {{/each}}
      {{#with obj }} other text
      {{*parent.var.a(a: "b").a.x}} {{/with}}
      """

roundTrip :: forall m g. Monad m => MonadThrow Error g => String -> SpecT g Unit m Unit
roundTrip src = roundTripNamed [] (trim src) src

roundTripNamed
  :: forall m g
   . Monad m
  => MonadThrow Error g
  => Array (List (Ast Positions) -> Boolean)
  -> String
  -> String
  -> SpecT g Unit m Unit
roundTripNamed astTests name src = it ("round trip - " <> name) do
  case parse src of
    Left err -> throwError $ error $ parseErrorMessage err
    Right parsed -> do
      when (not all (\t -> t parsed) astTests) do
        throwError $ error $ "Failed ast tests: \n\n" <> src <> "\n\n with ast: \n" <> show parsed <> "\n\n"
      let printed = printTemplateAsts parsed
      when (printed /= src) do
        throwError $ error $ "Failed to round trip: \n\n" <> src <> "\n\n -> \n\n" <> printed <> "\n\n with ast: \n" <> show parsed <> "\n\n"

hasEach :: List (Ast Positions) -> Boolean
hasEach = any
  case _ of
    Ast.Each _ _ _ -> true
    _ -> false

hasWith :: List (Ast Positions) -> Boolean
hasWith = any
  case _ of
    Ast.With _ _ _ -> true
    _ -> false
