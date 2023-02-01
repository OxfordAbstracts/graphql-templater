module Test.GraphQL.Templater.Ast.Suggest (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.GraphQL.Parser (document)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect.Exception (Error, error)
import GraphQL.Templater.Ast.Parser (parse)
import GraphQL.Templater.Ast.Suggest (getPathAt, suggestEaches)
import GraphQL.Templater.TypeDefs (GqlTypeTree, getTypeTreeFromDoc)
import Parsing (ParseError, parseErrorMessage, runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.Ast.Suggest" do
    describe "getPathAt" do
      it "should return an empty path when text only" do
        let
          template = "this is text"

        ast <- throwParser $ parse template
        getPathAt 3 ast `shouldEqual` Nil

      it "should return a path in an each" do
        let
          template = "{{#each x}} this is text{{/each}}"

        ast <- throwParser $ parse template
        getPathAt 14 ast `shouldEqual` pure "x"

      it "should return a path the 2nd each" do
        let
          template = "{{#each a}}{{/each}}{{#each b}} this is text{{/each}}"

        ast <- throwParser $ parse template
        getPathAt 35 ast `shouldEqual` pure "b"

      it "should return a path in a nested each" do
        let
          template = "{{#each x}}{{#each y}}this is text{{/each}}{{/each}}"

        ast <- throwParser $ parse template
        getPathAt 25 ast `shouldEqual` ("x" : "y" : Nil)

      it "should return a path in a with in an each" do
        let
          template = "{{#each x}}{{#with y}}this is text{{/with}}{{/each}}"

        ast <- throwParser $ parse template
        getPathAt 25 ast `shouldEqual` ("x" : "y" : Nil)
    describe "suggestEaches" do
      it "should return an empty list when there are no eaches in the schema" do
        let
          template = "this is text"

        ast <- throwParser $ parse template
        tree <- parseTypeTree simpleSchema
        suggestEaches 1 ast tree `shouldEqual` Nil

      it "should return top level eaches" do
        let
          template = "this is text"

        ast <- throwParser $ parse template
        tree <- parseTypeTree usersSchema
        suggestEaches 1 ast tree `shouldEqual` pure "users"

      it "should return eaches inside eaches" do
        let
          template = "{{#each users}} this is text{{/each}}"

        ast <- throwParser $ parse template
        tree <- parseTypeTree usersSchema
        suggestEaches 15 ast tree `shouldEqual` pure "friends"

parseTypeTree :: forall m. MonadThrow Error m => String -> m GqlTypeTree
parseTypeTree schema = case runParser schema document of
  Left errs -> throwError $ error $ parseErrorMessage errs
  Right doc -> case getTypeTreeFromDoc doc of
    Nothing -> throwError $ error "No type tree"
    Just tree -> pure tree

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

throwParser :: forall m a. MonadThrow Error m => Either ParseError a -> m a
throwParser = case _ of
  Left errs -> throwError $ error $ parseErrorMessage errs
  Right a -> pure a

