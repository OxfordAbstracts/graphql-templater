module Test.GraphQL.Templater.Ast.Print where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.List (List, all, any)
import Data.String (trim)
import Effect.Exception (Error)
import GraphQL.Templater.Ast (Ast)
import GraphQL.Templater.Ast as Ast
import GraphQL.Templater.Ast.Print (printPositioned, printUnpositioned)
import GraphQL.Templater.Parser (parse)
import GraphQL.Templater.Positions (Positions)
import Parsing (parseErrorMessage)
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Ast.Print" do
    describe "printPositioned & printUnpositioned" do
      roundTripSimple "text only"

      roundTripSimple " text with whitespace "

      roundTripSimple "\n text with newlines \n"

      roundTripSimple " \n text with newlines and whitespace \n "

      roundTripSimple "var {{variable}}"

      roundTripSimple "chained {{chained.variable}}"

      roundTripSimple "parents and roots {{chained.*parent.*root}}"

      roundTripSimple "arg {{variable(arg1: 1)}}"

      roundTripSimple "args {{variable(arg1: 1, arg2: 2)}}"

      roundTripSimple "args on args {{chained(b: true).variable(arg1: 1, arg2: 2)}}"

      roundTrip [ hasEach ] "eaches" """{{#each array }}text{{/each}}"""

      roundTrip [ hasEach ] "eaches on newlines"
        """{{#each array }} 
      text{{/each}} """

      roundTrip [ hasEach ] "eaches with variables"
        """{{#each array }}
      {{var.a}} {{/each}}"""

      roundTrip [ hasWith ] "withs with variables with args"
        """{{#with obj }}
      {{var.a(a: "b")}} {{/with}}"""

      roundTrip [ hasEach, hasWith ] "a complex example"
        """{{#each items}}
      {{#each inner-items}}{{var.a(a: "b")}}{{/each}} some text {{/each}}
      {{#with obj }} other text
      {{*parent.var.a(a: "b").a.x}} {{/with}}
      """

roundTripSimple :: forall m g. Monad m => MonadThrow Error g => String -> SpecT g Unit m Unit
roundTripSimple src = roundTrip [] (trim src) src

roundTrip
  :: forall m g
   . Monad m
  => MonadThrow Error g
  => Array (List (Ast Positions) -> Boolean)
  -> String
  -> String
  -> SpecT g Unit m Unit
roundTrip astTests name src = do
  it ("printPositioned round trip - " <> name) do
    case parse src of
      Left err -> fail $ parseErrorMessage err
      Right parsed -> do
        when (not all (\t -> t parsed) astTests) do
          fail $ "Failed ast tests: \n\n" <> src <> "\n\n with ast: \n" <> show parsed <> "\n\n"
        let printed = printPositioned parsed
        when (printed /= src) do
          fail $
            "Failed to round trip: \n\n"
              <> src
              <> "\n\n -> \n\n"
              <> printed
              <> "\n\n with ast: \n"
              <> show parsed
              <> "\n\n"

  it ("printUnpositioned round trip - " <> name) do
    case parse src of
      Left err -> fail $ parseErrorMessage err
      Right parsed -> do
        let printed = printUnpositioned parsed
        case parse printed of
          Left err -> fail $ parseErrorMessage err
          Right reparsed -> do
            when (map (map (const unit)) parsed /= map (map (const unit)) reparsed) do
              fail $
                "Failed to round trip: \n\n"
                  <> src
                  <> "\n\n -> \n\n"
                  <> printed
                  <> "\n\n with ast: \n"
                  <> show parsed
                  <> "\n\n"

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
