module Test.GraphQL.Templater.Ast.Transform where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import GraphQL.Templater.Ast (Ast(..))
import GraphQL.Templater.Ast.Transform (insertTextAt)
import Parsing (Position(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

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
