module Test.GraphQL.Templater.Ast.Transform
  ( spec
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.List (List(..), foldMap, (:))
import Data.Maybe (Maybe(..), maybe)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import GraphQL.Templater.Ast (Ast(..))
import GraphQL.Templater.Ast.Parser (parse)
import GraphQL.Templater.Ast.Print (printPositioned)
import GraphQL.Templater.Ast.Transform (insertEmptyEachAt, insertTextAt)
import Parsing (Position(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Ast.Transform" do
    describe "insertTextAt" do
      it "should insert text in a single text ast" do
        insertTextAt " new " 1
          ( pure $ Text "ab"
              { start: Position { line: 1, column: 1, index: 0 }
              , end: Position { line: 1, column: 3, index: 2 }
              }
          )
          `shouldEqual`
            ( Just $ pure $ Text "a new b"
                { start: Position { line: 1, column: 1, index: 0 }
                , end: Position { line: 1, column: 8, index: 7 }
                }
            )
      it "should insert in the 2nd text" do

        insertTextAt "a" 3
          ( Text "12"
              { start: Position { line: 1, column: 1, index: 0 }
              , end: Position { line: 1, column: 3, index: 2 }
              }
              :
                Text "34"
                  { start: Position { line: 1, column: 3, index: 2 }
                  , end: Position { line: 1, column: 5, index: 4 }
                  }
              : Nil
          )
          `shouldEqual`
            ( Just $
                Text "12"
                  { start: Position { line: 1, column: 1, index: 0 }
                  , end: Position { line: 1, column: 3, index: 2 }
                  }
                  :
                    Text "3a4"
                      { start: Position { line: 1, column: 3, index: 2 }
                      , end: Position { line: 1, column: 6, index: 5 }
                      }
                  : Nil
            )

      it "should insert in the 1st text" do
        insertTextAt "a" 2
          ( Text "12"
              { start: Position { line: 1, column: 1, index: 0 }
              , end: Position { line: 1, column: 3, index: 2 }
              }
              :
                Text "34"
                  { start: Position { line: 1, column: 3, index: 2 }
                  , end: Position { line: 1, column: 5, index: 4 }
                  }
              : Nil
          )
          `shouldEqual`
            ( Just $
                Text "12a"
                  { start: Position { line: 1, column: 1, index: 0 }
                  , end: Position { line: 1, column: 4, index: 3 }
                  }
                  :
                    Text "34"
                      { start: Position { line: 1, column: 4, index: 3 }
                      , end: Position { line: 1, column: 6, index: 5 }
                      }
                  : Nil
            )

      it "should handle newlines" do
        insertTextAt "\n" 2
          ( Text "12"
              { start: Position { line: 1, column: 1, index: 0 }
              , end: Position { line: 1, column: 3, index: 2 }
              }
              :
                Text "34"
                  { start: Position { line: 1, column: 3, index: 2 }
                  , end: Position { line: 1, column: 5, index: 4 }
                  }
              : Nil
          )
          `shouldEqual`
            ( Just $
                Text "12\n"
                  { start: Position { line: 1, column: 1, index: 0 }
                  , end: Position { line: 2, column: 1, index: 3 }
                  }
                  :
                    Text "34"
                      { start: Position { line: 2, column: 1, index: 3 }
                      , end: Position { line: 2, column: 3, index: 5 }
                      }
                  : Nil
            )

      it "should insert into an each" do
        parseAndTestInsertTextAt "a" 15 "{{#each list}}  {{/each}}" "{{#each list}} a {{/each}}"

      it "should insert into an each with newlines" do
        parseAndTestInsertTextAt "a" 16 "\n{{#each list}}  \n{{/each}}\n" "\n{{#each list}} a \n{{/each}}\n"

      it "should insert at the at the beginning of a variable only template" do
        parseAndTestInsertTextAt "a" 0 "{{var}}" "a{{var}}"

      it "should insert at the at the end of a variable only template" do
        parseAndTestInsertTextAt "a" 7 "{{var}}" "{{var}}a"

    describe "insertEmptyEachAt" do
      it "should insert an empty each into a blank string" do
        parseAndTestInsertEmptyEachAt
          "list"
          1
          "  "
          " {{#each list}}{{/each}} "

parseAndTestInsertTextAt :: forall m. MonadEffect m => MonadThrow Error m => String -> Int -> String -> String -> m Unit
parseAndTestInsertTextAt insert idx input expected =
  case parse input, parse expected of
    Right inputParsed, Right expectedParsed -> do
      maybe "insertTextAt returned Nothing" printPositioned (insertTextAt insert idx inputParsed) `shouldEqual` printPositioned expectedParsed
    _, _ -> fail "failed to parse"

parseAndTestInsertEmptyEachAt :: forall m. MonadEffect m => MonadThrow Error m => String -> Int -> String -> String -> m Unit
parseAndTestInsertEmptyEachAt insert idx input expected =
  case parse input, parse expected of
    Right inputParsed, Right expectedParsed -> do
      foldMap printPositioned (insertEmptyEachAt insert idx inputParsed) `shouldEqual` printPositioned expectedParsed
    _, _ -> fail "failed to parse"
    
