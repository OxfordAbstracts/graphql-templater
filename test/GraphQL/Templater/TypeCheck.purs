module Test.GraphQL.Templater.TypeCheck (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.GraphQL.Parser (document)
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe)
import Effect.Exception (Error, error)
import GraphQL.Templater.JsonPos (NormalizedJsonPos(..))
import GraphQL.Templater.Parser (parse)
import GraphQL.Templater.TypeCheck (getTypeErrorsFromTree)
import GraphQL.Templater.TypeCheck.Errors (ArgTypeError(..), PositionedError, TypeError(..), TypeErrorWithPath(..))
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), getTypeTreeFromDoc)
import Parsing (ParseError, Position(..), parseErrorMessage, runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.TypeDefs" do
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
          ( TypeErrorWithPath FieldNotFound
              ( ( Key "bar"
                    { end: (Position { column: 10, index: 9, line: 1 })
                    , start: (Position { column: 7, index: 6, line: 1 })
                    }
                ) : Nil
              )
              { end: (Position { column: 10, index: 9, line: 1 })
              , start: (Position { column: 7, index: 6, line: 1 })
              } : Nil
          )
      it "should return multiple errors for multiple incorrect field lookups" do
        let
          template = "text{{bar}}text{{baz}}text"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual`
          ( TypeErrorWithPath FieldNotFound
              ( (Key "baz" unit) : Nil
              )
              unit
              : TypeErrorWithPath FieldNotFound
                  ( (Key "bar" unit) : Nil
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
          ( TypeErrorWithPath FieldNotFound
              ( Key "user" unit
                  : Key "not_here" unit
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

      it "should return an error if the each field is not a list" do
        let
          template = "{{#each user(id: 1)}}{{id}}{{/each}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual` ((TypeErrorWithPath NotList ((Key "user" unit) : Nil) unit) : Nil)

      it "should return an error if variable inside an each is not a child of the each" do
        let
          template = "{{#each users}}{{top_level}}{{/each}}"

        errors <- typeCheckNoPos usersSchema template
        errors `shouldEqual`
          ( ( TypeErrorWithPath FieldNotFound
                ( (Key "users" unit)
                    : (Key "top_level" unit)
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
          ( ( TypeErrorWithPath FieldNotFound
                ( (Key "user" unit)
                    : (Key "friends" unit)
                    : (Key "friends" unit)
                    : (Key "invalid" unit)
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
                          { end: (Position { column: 16, index: 15, line: 1 })
                          , start: (Position { column: 9, index: 8, line: 1 })
                          }
                      )
                  )
                  ( ( Key "users"
                        { end: (Position { column: 8, index: 7, line: 1 })
                        , start: (Position { column: 3, index: 2, line: 1 })
                        }
                    )
                      :
                        ( Key "id"
                            { end: (Position { column: 23, index: 22, line: 1 })
                            , start: (Position { column: 21, index: 20, line: 1 })
                            }
                        )
                      : Nil
                  )
                  { end: (Position { column: 8, index: 7, line: 1 })
                  , start: (Position { column: 3, index: 2, line: 1 })
                  }
              ) : Nil
            )
          )

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
  pure $ getTypeErrorsFromTree (fromMaybe GqlUndefined $ getTypeTreeFromDoc doc) asts

throwParser :: forall m a. MonadThrow Error m => Either ParseError a -> m a
throwParser = case _ of
  Left errs -> throwError $ error $ parseErrorMessage errs
  Right a -> pure a
