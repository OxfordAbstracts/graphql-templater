module Test.GraphQL.Templater.Ast.Transform
  ( spec
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.List (List(..), foldMap, (:))
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import GraphQL.Templater.Ast (Ast(..))
import GraphQL.Templater.Ast.Print (printPositioned)
import GraphQL.Templater.Ast.Transform (insertTextAt)
import GraphQL.Templater.Parser (parse)
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

parseAndTestInsertTextAt :: forall m. MonadEffect m => MonadThrow Error m => String -> Int -> String -> String -> m Unit
parseAndTestInsertTextAt insert idx input expected =
  case parse input, parse expected of
    Right inputParsed, Right expectedParsed -> do
      foldMap printPositioned (insertTextAt insert idx inputParsed) `shouldEqual` printPositioned expectedParsed
    _, _ -> fail "failed to parse"

-- x =
--   ( Just
--       ( ( Each
--             ( VarPath
--                 ( NonEmptyList
--                     ( NonEmpty
--                         ( VarPathPart
--                             { args: Nothing, name: (VarPartNameGqlName "list" { end: (Position { column: 13, index: 12, line: 1 }), start: (Position { column: 9, index: 8, line: 1 }) }) }
--                             { end: (Position { column: 13, index: 12, line: 1 })
--                             , start: (Position { column: 9, index: 8, line: 1 })
--                             }
--                         )
--                         Nil
--                     )
--                 )
--                 { end: (Position { column: 13, index: 12, line: 1 })
--                 , start: (Position { column: 9, index: 8, line: 1 })
--                 }
--             )
--             ( ( Text " a "
--                   { end: (Position { column: 4, index: 17, line: 1 })
--                   , start: (Position { column: 15, index: 14, line: 1 })
--                   }
--               ) : Nil
--             )
--             { end: (Position { column: 13, index: 25, line: 1 })
--             , start: (Position { column: 1, index: 0, line: 1 })
--             }
--         ) : Nil
--       )
--   )

-- x =
--   ( ( Each
--         ( VarPath
--             ( NonEmptyList
--                 ( NonEmpty
--                     ( VarPathPart
--                         { args: Nothing
--                         , name:
--                             ( VarPartNameGqlName "list"
--                                 { end: (Position { column: 13, index: 12, line: 1 })
--                                 , start: (Position { column: 9, index: 8, line: 1 })
--                                 }
--                             )
--                         }
--                         { end: (Position { column: 13, index: 12, line: 1 })
--                         , start: (Position { column: 9, index: 8, line: 1 })
--                         }
--                     )
--                     Nil
--                 )
--             )
--             { end: (Position { column: 13, index: 12, line: 1 })
--             , start: (Position { column: 9, index: 8, line: 1 })
--             }
--         )
--         ( ( Text "  "
--               { end: (Position { column: 17, index: 16, line: 1 })
--               , start: (Position { column: 15, index: 14, line: 1 })
--               }
--           ) : Nil
--         )
--         { end: (Position { column: 26, index: 25, line: 1 })
--         , start: (Position { column: 1, index: 0, line: 1 })
--         }
--     ) : Nil
--   )
