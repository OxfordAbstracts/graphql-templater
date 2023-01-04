module Test.GraphQL.Templater.Eval.Interpolate
  ( spec
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.List (List(..), (:))
import Data.List.Types (nelCons)
import Data.Maybe (Maybe(..))
import GraphQL.Templater.Ast (Ast(..), VarPartName(..), VarPath(..), VarPathPart(..))
import GraphQL.Templater.Eval.Interpolate (interpolate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Eval.Interpolate" do
    describe "interpolate" do
      it "should return the same text when there are no variables" do
        pure unit
        interpolate Nil testJson `shouldEqual` ""
        interpolate (Text "test" unit : Nil) testJson `shouldEqual` "test"

      it "should interpolate top level variables" do
        pure unit
        interpolate
          ( mkVar "top_level_1"
              : Text " " unit
              : mkVar "top_level_true"
              : Nil
          )
          testJson `shouldEqual` "1 true"

      it "should interpolate nested variables" do
        pure unit
        interpolate
          ( mkVarNested ("obj" `nelCons` pure "a")
              : Text " " unit
              : mkVarNested ("obj" `nelCons` pure "b")
              : Nil
          )
          testJson `shouldEqual` "a b"
      it "should interpolate eaches" do
        pure unit
        interpolate
          ( Each (mkPath $ pure "arr")
              ( Text " start " unit
                  : mkVar "id"
                  : Text " " unit
                  : mkVar "x"
                  : Text " end " unit
                  : Nil
              )
              unit
              : Nil
          )
          testJson `shouldEqual` " start 1 a end  start 2 b end "

      it "should interpolate eaches with multilevel lookups" do
        pure unit
        interpolate
          ( Each (mkPath $ pure "arr")
              ( mkVarNested ("obj" `nelCons` pure "a")
                  : Nil
              )
              unit
              : Nil
          )
          testJson `shouldEqual` "aa"

      it "should interpolate nested eaches" do
        pure unit
        interpolate
          ( Each (mkPath $ pure "arr")
              ( Text " start " unit
                  : mkVar "id"
                  : Text " " unit
                  : mkVar "x"
                  : Text " end " unit
                  : Each (mkPath $ pure "children")
                      ( Text " start " unit
                          : mkVar "y"
                          : Text " end " unit
                          : Nil
                      )
                      unit
                  : Nil
              )
              unit
              : Nil
          )
          testJson `shouldEqual` " start 1 a end  start a a end  start a b end  start 2 b end  start b a end  start b b end "

      it "should interpolate parents from an each" do
        interpolate
          ( Each (mkPath $ pure "arr")
              ( Var
                  ( mkPath' $
                      { args: Nothing
                      , name: VarPartNameParent unit
                      }
                        `nelCons` pure
                          { args: Nothing
                          , name: VarPartNameGqlName "top_level_true" unit
                          }
                  )
                  unit
                  : Nil
              )
              unit
              : Nil
          )
          testJson `shouldEqual` "truetrue"

      it "should interpolate parents from a nested each" do
        interpolate
          ( Each (mkPath $ pure "arr")
              ( Each (mkPath $ pure "children")
                  ( Var
                      ( mkPath' $
                          { args: Nothing
                          , name: VarPartNameParent unit
                          }
                            `nelCons` pure
                              { args: Nothing
                              , name: VarPartNameGqlName "id" unit
                              }
                      )
                      unit
                      : Nil
                  )
                  unit : Nil
              )
              unit
              : Nil
          )
          testJson `shouldEqual` "1122"

      it "should interpolate roots from a nested each" do
        interpolate
          ( Each (mkPath $ pure "arr")
              ( Each (mkPath $ pure "children")
                  ( Var
                      ( mkPath' $
                          { args: Nothing
                          , name: VarPartNameRoot unit
                          }
                            `nelCons` pure
                              { args: Nothing
                              , name: VarPartNameGqlName "top_level_1" unit
                              }
                      )
                      unit
                      : Nil
                  )
                  unit : Nil
              )
              unit
              : Nil
          )
          testJson `shouldEqual` "1111"
      it "should lookup up the first object in an array when an object lookup is on an array" do
        interpolate
          ( mkVarNested ("arr" `nelCons` pure "id")
              : Nil
          )
          testJson `shouldEqual` "1"

  where
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

testJson :: Json
testJson = encodeJson
  { top_level_1: 1
  , top_level_true: true
  , obj: { a: "a", b: "b" }
  , arr:
      [ { id: 1
        , x: "a"
        , obj: { a: "a", b: "b" }
        , children:
            [ { y: "a a" }
            , { y: "a b" }
            ]
        }
      , { id: 2
        , x: "b"
        , obj: { a: "a", b: "b" }
        , children:
            [ { y: "b a" }
            , { y: "b b" }
            ]
        }
      ]
  }