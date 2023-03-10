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
import GraphQL.Templater.Ast (Ast(..), AstPos, VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Ast.Parser (parse)
import GraphQL.Templater.Ast.Print (printPositioned)
import GraphQL.Templater.Ast.Transform (insertEmptyEachAt, insertTextAt, modifyAstStartingAt)
import GraphQL.Templater.Positions (Positions)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Ast.Transform" do
    describe "insertTextAt" do
      it "should insert text in a single text ast" do
        insertTextAt " new " 1
          ( pure $ Text "ab"
              { start: 0
              , end: 2
              , str: "ab"
              }
          )
          `shouldEqual`
            ( Just $ pure $ Text "a new b"
                { start: 0
                , end: 7
                , str: "a new b"
                }
            )
      it "should insert in the 2nd text" do

        insertTextAt "a" 3
          ( Text "12"
              { start: 0
              , end: 2
              , str: "12"
              }
              :
                Text "34"
                  { start: 2
                  , end: 4
                  , str: "34"
                  }
              : Nil
          )
          `shouldEqual`
            ( Just $
                Text "12"
                  { start: 0
                  , end: 2
                  , str: "12"
                  }
                  :
                    Text "3a4"
                      { start: 2
                      , end: 5
                      , str: "3a4"
                      }
                  : Nil
            )

      it "should insert in the 1st text" do
        insertTextAt "a" 2
          ( Text "12"
              { start: 0
              , end: 2
              , str: "12"
              }
              :
                Text "34"
                  { start: 2
                  , end: 4
                  , str: "34"
                  }
              : Nil
          )
          `shouldEqual`
            ( Just $
                Text "12a"
                  { start: 0
                  , end: 3
                  , str: "12a"
                  }
                  :
                    Text "34"
                      { start: 3
                      , end: 5
                      , str: "34"
                      }
                  : Nil
            )

      it "should handle newlines" do
        insertTextAt "\n" 2
          ( Text "12"
              { start: 0
              , end: 2
              , str: "12"
              }
              :
                Text "34"
                  { start: 2
                  , end: 4
                  , str: "34"
                  }
              : Nil
          )
          `shouldEqual`
            ( Just $
                Text "12\n"
                  { start: 0
                  , end: 3
                  , str: "12\n"
                  }
                  :
                    Text "34"
                      { start: 3
                      , end: 5
                      , str: "34"
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

    describe "modifyAstStartingAt" do
      it "should replace a text only ast" do
        parseAndTestModifyAstStartingAt (const $ pure $ Text "new" Nothing) 0 "old" "new"

      it "should replace a var" do
        parseAndTestModifyAstStartingAt
          ( const $ pure $ Var
              ( VarPath
                  ( pure $ VarPathPart
                      { args: Nothing
                      , name: VarPartNameGqlName "newvar" Nothing
                      }
                      Nothing
                  )
                  Nothing
              )
              Nothing
          )
          6
          "before{{var}}after"
          "before{{newvar}}after"

      it "should delete a var" do
        parseAndTestModifyAstStartingAt
          ( const Nil
          )
          6
          "before{{var}}after"
          "beforeafter"

      it "should double a var" do
        parseAndTestModifyAstStartingAt
          ( \ast -> map Just <$> pure ast <> pure ast
          )
          6
          "before{{var}}after{{othervar}}"
          "before{{var}}{{var}}after{{othervar}}"

      it "should delete a var in an each" do
        parseAndTestModifyAstStartingAt
          ( const Nil
          )
          20
          "{{#each list}}before{{var}}after{{/each}}}"
          "{{#each list}}beforeafter{{/each}}}"

      it "should double a var in a with" do
        parseAndTestModifyAstStartingAt
          ( \ast -> map Just <$> pure ast <> pure ast
          )
          20
          "{{#with objt}}before{{var}}after{{/with}}}"
          "{{#with objt}}before{{var}}{{var}}after{{/with}}}"

parseAndTestInsertTextAt :: forall m. MonadEffect m => MonadThrow Error m => String -> Int -> String -> String -> m Unit
parseAndTestInsertTextAt insert idx input expected =
  case parse input, parse expected of
    Right inputParsed, Right expectedParsed -> do
      maybe "insertTextAt returned Nothing" printPositioned (insertTextAt insert idx inputParsed)
        `shouldEqual` printPositioned expectedParsed
    _, _ -> fail "failed to parse"

parseAndTestInsertEmptyEachAt :: forall m. MonadEffect m => MonadThrow Error m => String -> Int -> String -> String -> m Unit
parseAndTestInsertEmptyEachAt insert idx input expected =
  case parse input, parse expected of
    Right inputParsed, Right expectedParsed -> do
      foldMap printPositioned (insertEmptyEachAt insert idx inputParsed) `shouldEqual` printPositioned expectedParsed
    _, _ -> fail "failed to parse"

parseAndTestModifyAstStartingAt
  :: forall m p
   . MonadThrow Error m
  => (AstPos -> List (Ast (Maybe Positions)))
  -> Int
  -> String
  -> String
  -> m Unit
parseAndTestModifyAstStartingAt fn idx input expected =
  case parse input, parse expected of
    Right inputParsed, Right expectedParsed -> do
      printPositioned (modifyAstStartingAt fn idx inputParsed) `shouldEqual` printPositioned expectedParsed
    _, _ -> fail "failed to parse"