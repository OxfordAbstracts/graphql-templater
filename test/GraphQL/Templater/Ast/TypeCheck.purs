module Test.GraphQL.Templater.Ast.TypeCheck (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.GraphQL.Parser (document)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Set as Set
import Effect.Exception (Error, error)
import GraphQL.Templater.Ast.Argument (ArgName(..), Argument(..), T_Argument, Value(..))
import GraphQL.Templater.Ast.Parser (parse)
import GraphQL.Templater.Ast.TypeCheck (getTypeErrorsFromTree)
import GraphQL.Templater.Ast.TypeCheck.Errors (ArgTypeError(..), PositionedError, TypeError(..), TypeErrorWithPath(..))
import GraphQL.Templater.Eval.MakeQuery (getAlias)
import GraphQL.Templater.JsonPos (NormalizedJsonPos(..))
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), getTypeTreeFromDoc)
import Parsing (ParseError, parseErrorMessage, runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Ast.TypeCheck" do
    describe "getTypeErrorsFromTree" do
      it "should return no errors for a simple valid schema and an empty template" do
        let
          template = ""

        errors <- throwParser $ typeCheck simpleSchema template
        errors `shouldEqual` Nil

      it "should return no errors for a simple valid schema and a text only template" do
        let
          template = "text"

        errors <- throwParser $ typeCheck simpleSchema template
        errors `shouldEqual` Nil

      it "should return no errors for a simple valid schema and a simple template" do
        let
          template = "text{{foo}}text"

        errors <- throwParser $ typeCheck simpleSchema template
        errors `shouldEqual` Nil

      it "should return an error for an incorrect field lookup" do
        let
          template = "text{{bar}}text"

        errors <- throwParser $ typeCheck simpleSchema template
        errors `shouldEqual`
          ( TypeErrorWithPath (FieldNotFound $ Set.fromFoldable [ "foo" ])
              ( ( Key { name: "bar", alias: Nothing }
                    { end: 9
                    , start: 6
                    , str: "bar"
                    }
                ) : Nil
              )
              { end: 9
              , start: 6
              , str: "bar"
              } : Nil
          )
      it "should return multiple errors for multiple incorrect field lookups" do
        let
          template = "text{{bar}}text{{baz}}text"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual`
          ( TypeErrorWithPath (FieldNotFound $ Set.fromFoldable [ "user", "users", "top_level" ])
              ( (Key (noAlias "baz") unit) : Nil
              )
              unit
              : TypeErrorWithPath (FieldNotFound $ Set.fromFoldable [ "user", "users", "top_level" ])
                  ( (Key (noAlias "bar") unit) : Nil
                  )
                  unit
              : Nil
          )

      it "should return no errors for a nested lookup in a recursive schema" do
        let
          template = "text{{users.id}}text"

        errors <- throwParser $ typeCheck usersSchema template
        errors `shouldEqual` Nil

      it "should return no errors for a nested lookup with arguments in a recursive schema" do
        let
          template = "text{{user(id: \"id-val\").id}}text"

        errors <- throwParser $ typeCheck usersSchema template
        errors `shouldEqual` Nil
      it "should return an error for a nested lookup with arguments when the field does not exist" do
        let
          template = "text{{user(id: \"id-val\").not_here}}text"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual`
          ( TypeErrorWithPath (FieldNotFound $ Set.fromFoldable [ "id", "name", "friends" ])
              ( Key
                  ( withAlias "user" $ pure
                      { name: ArgName "id" unit
                      , value: Value_StringValue (wrap "id-val") unit
                      , pos: unit
                      }
                  )
                  unit
                  : Key (noAlias "not_here") unit
                  : Nil
              )
              unit : Nil
          )

      it "should return no errors with a valid each" do
        let
          template = "{{#each users}}{{id}}{{/each}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual` Nil

      it "should return no errors with a valid each using parent" do
        let
          template = "{{#each users}}{{*parent.top_level}}{{/each}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual` Nil

      it "should return no errors with a valid \"with\"" do
        let
          template = "{{#with user(id:1)}}{{id}}{{/with}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual` Nil

      it "should return no errors with a valid each using parent" do
        let
          template = "{{#each users}}{{*parent.top_level}}{{/each}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual` Nil

      it "should return an error if the each field is not a list" do
        let
          template = "{{#each user(id: 1)}}{{id}}{{/each}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual`
          ( ( TypeErrorWithPath NotList
                ( ( Key
                      ( withAlias "user" $ pure
                          { name: ArgName "id" unit
                          , value: Value_IntValue (wrap 1) unit
                          , pos: unit
                          }
                      )
                      unit
                  ) : Nil
                )
                unit
            ) : Nil
          )

      it "should return an error if the with field is not a object" do
        let
          template = "{{#with users}}{{id}}{{/with}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual` ((TypeErrorWithPath NotObject ((Key (noAlias "users") unit) : Nil) unit) : Nil)

      it "should return an error if variable inside an each is not a child of the each" do
        let
          template = "{{#each users}}{{top_level}}{{/each}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual`
          ( ( TypeErrorWithPath (FieldNotFound $ Set.fromFoldable [ "friends", "id", "name" ])
                ( (Key (noAlias "users") unit)
                    : (Key (noAlias "top_level") unit)
                    : Nil
                )
                unit
            ) : Nil
          )

      it "should return an error for an invalid nested variable in an each" do
        let
          template = "{{#each user(id: 1).friends}}{{friends.invalid}}{{/each}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual`
          ( ( TypeErrorWithPath (FieldNotFound $ Set.fromFoldable [ "friends", "id", "name" ])
                ( ( Key
                      ( withAlias "user" $ pure
                          { name: ArgName "id" unit
                          , value: Value_IntValue (wrap 1) unit
                          , pos: unit
                          }
                      )
                      unit
                  )
                    : (Key (noAlias "friends") unit)
                    : (Key (noAlias "friends") unit)
                    : (Key (noAlias "invalid") unit)
                    : Nil
                )
                unit
            ) : Nil
          )

      it "should return an error for a not found argument" do
        let
          template = "{{users(invalid: 1).id}}"

        errors <- throwParser $ typeCheck usersSchema template

        errors `shouldEqual`

          ( ( ( TypeErrorWithPath
                  ( ArgTypeError
                      ( ArgUnknown "invalid"
                          { end: 15
                          , start: 8
                          , str: "invalid"
                          }
                      )
                  )
                  ( ( Key
                        ( withAlias "users" $ pure
                            { name: ArgName "invalid" unit
                            , value: Value_IntValue (wrap 1) unit
                            , pos: unit
                            }
                        )
                        { end: 7
                        , start: 2
                        , str: "users"
                        }
                    )
                      :
                        ( Key (noAlias "id")
                            { end: 22
                            , start: 20
                            , str: "id"
                            }
                        )
                      : Nil
                  )
                  { end: 7
                  , start: 2
                  , str: "users"
                  }
              ) : Nil
            )
          )

noAlias ∷ ∀ (n ∷ Type) (a ∷ Type). n → { alias ∷ Maybe a, name ∷ n }
noAlias = { name: _, alias: Nothing }

withAlias
  :: forall a
   . String
  -> List (T_Argument a)
  -> { alias :: Maybe String
     , name :: String
     }
withAlias name args = { name, alias: getAlias name (map Argument args) }

-- withAlias 

simpleSchema ∷ String
simpleSchema =
  """
  type Query {
    foo: String
  }
  """

usersSchema ∷ String
usersSchema =
  """
  type Query {
    user(id: ID!): User
    users: [User]
    top_level: String
  }

  type User {
    id: ID!
    name: String
    friends: [User]
  }
  """

-- typeCheckNoPos :: String -> String -> Either ParseError Unit
typeCheckNoPos :: forall m. MonadThrow Error m => String -> String -> m (List (TypeErrorWithPath Unit))
typeCheckNoPos schema template = throwParser $ removePos $ typeCheck schema template

removePos :: Either ParseError (List PositionedError) -> Either ParseError (List (TypeErrorWithPath Unit))
removePos = map (map (map (const unit)))

typeCheck :: String -> String -> Either ParseError (List PositionedError)
typeCheck schema template = do
  doc <- runParser schema document
  asts <- parse template
  pure $ getTypeErrorsFromTree (fromMaybe (Node "Could not make tree") $ getTypeTreeFromDoc doc) asts

throwParser :: forall m a. MonadThrow Error m => Either ParseError a -> m a
throwParser = case _ of
  Left errs -> throwError $ error $ parseErrorMessage errs
  Right a -> pure a
