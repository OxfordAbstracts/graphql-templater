module Test.GraphQL.Templater.Parser
  ( spec
  ) where

import Prelude

import Data.Either (Either(..))
import Data.GraphQL.AST (Value(..))
import Data.List.Types (List(..), nelCons, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import GraphQL.Templater.Ast (Arg(..), ArgName(..), Ast(..), Value(..), VarPartName(..), VarPath(..), VarPathPart(..))
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
                  , args: Just $
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

                  }
                    `nelCons` pure
                      { name: VarPartNameGqlName "b" unit
                      , args: Just $
                          ( mkArgs $
                              { name: "x"
                              , value: Value_BooleanValue $ wrap true
                              }
                                : Nil
                          )

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
              unit unit
              : Nil
          )
      it "should parse nested eaches" do
        parseWoPos "{{#each a}}{{#each b}}text{{/each}}{{/each}}" `shouldEqual` Right
          ( Each (mkPath $ pure "a")
              ( Each (mkPath $ pure "b")
                  ( Text "text" unit
                      : Nil
                  )
                  unit unit
                  : Nil
              )
              unit unit
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

  mkArgs = map \{ name, value } ->
    ( Arg
        { name: ArgName name unit
        , value: Value value unit
        }
        unit
    )

line1
  :: Int
  -> Int
  -> Positions
line1 start end =
  { start: Position { line: 1, column: start, index: start - 1 }
  , end: Position { line: 1, column: end, index: end - 1 }
  }
