module Test.GraphQL.Templater.Parser
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.GraphQL.AST (Argument(..), Arguments(..), Value(..))
import Data.List.Types (List(..), nelCons, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Parser (parse)
import GraphQL.Templater.Positions (Positions)
import Parsing (Position(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Parser" do
    describe "parse" do
      it "should return `Text` when there are no variables" do
        parse "test" `shouldEqual` Right (pure $ Text "test" (line1 1 5))

      it "should parse simple variables" do
        parseWoPos "{{test}}" `shouldEqual` Right
          ( pure $ mkVar "test"
          )

      it "should parse nested variables" do
        parseWoPos "{{a.b}}" `shouldEqual` Right
          ( pure $ mkVarNested $ "a" `nelCons` pure "b"
          )
      it "should parse nested variables with arguments" do
        parseWoPos "{{a(arg1: 1, arg2: \"x\").b(x: true)}}" `shouldEqual` Right
          ( pure $ Var
              ( mkPath' $
                  { name: VarPartNameGqlName "a" unit
                  , args: Just $ Tuple
                      ( mkArgs $
                          { name: "arg1"
                          , value: Value_IntValue $ wrap 1
                          }
                            :
                              { name: "arg2"
                              , value: Value_StringValue $ wrap "x"
                              }
                            : Nil
                      )
                      unit
                  }
                    `nelCons` pure
                      { name: VarPartNameGqlName "b" unit
                      , args: Just $ Tuple
                          ( mkArgs $
                              { name: "x"
                              , value: Value_BooleanValue $ wrap true
                              }
                                : Nil
                          )
                          unit
                      }
              )
              unit
          )

      it "should parse eaches" do
        parseWoPos "{{#each a}}text{{/each}}" `shouldEqual` Right
          ( Each (mkPath $ pure "a")
              ( Text "text" unit
                  : Nil
              )
              unit
              : Nil
          )
      it "should parse nested eaches" do
        parseWoPos "{{#each a}}{{#each b}}text{{/each}}{{/each}}" `shouldEqual` Right
          ( Each (mkPath $ pure "a")
              ( Each (mkPath $ pure "b")
                  ( Text "text" unit
                      : Nil
                  )
                  unit
                  : Nil
              )
              unit
              : Nil
          )
  where
  parseWoPos = parse >>> map (map (map (const unit)))

  mkVar = mkVarNested <<< pure

  mkVarNested strs = Var (mkPath strs) unit

  mkPath strs = VarPath
    ( strs <#> \str ->
        ( VarPathPart
            { args: Nothing
            , name: VarPartNameGqlName str unit
            }
            unit
        )
    )
    unit

  mkPath' parts = VarPath
    ( parts <#> \part ->
        ( VarPathPart
            part
            unit
        )
    )
    unit

  mkArgs = Arguments <<< map Argument

line1
  :: Int
  -> Int
  -> Positions
line1 start end =
  { start: Position { line: 1, column: start, index: start - 1 }
  , end: Position { line: 1, column: end, index: end - 1 }
  }

-- x =
--   ( Right
--       ( ( Text "test"
--             { end:
--                 (Position { column: 5, index: 4, line: 1 })
--             , start: (Position { column: 1, index: 0, line: 1 })
--             }
--         ) : Nil
--       )
--   )