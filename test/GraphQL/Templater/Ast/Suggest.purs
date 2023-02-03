module Test.GraphQL.TemplaterAst.Suggest (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Either (Either(..))
import Data.GraphQL.Parser (document)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect.Exception (Error, error)
import GraphQL.Templater.Ast (AstPos)
import GraphQL.Templater.Ast.Parser (parse)
import GraphQL.Templater.TypeDefs (GqlTypeTree, getTypeTreeFromDoc)
import GraphQL.TemplaterAst.Suggest (getPathAt, getStartingSuggestions)
import Parsing (ParseError, parseErrorMessage, runParser)
import Record.Extra (mapRecord)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "GraphQL.TemplaterAst.Suggest" do
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
    describe "getStartingSuggestions" do
      it "should return a single var when that is all there is" do
        let
          template = "this is text"

        ast <- throwParser $ parse template
        tree <- parseTypeTree simpleSchema
        getStartingSuggestions' 1 ast tree `shouldEqual`
          { eaches: Nil
          , vars: pure "foo"
          }

      it "should return top level eaches and vars" do
        let
          template = "this is text"

        ast <- throwParser $ parse template
        tree <- parseTypeTree usersSchema
        getStartingSuggestions' 1 ast tree `shouldEqual`
          { eaches: pure "users"
          , vars: "top_level" : "user" : Nil
          }

      it "should return eaches and vars inside eaches" do
        let
          template = "{{#each users}} this is text{{/each}}"

        ast <- throwParser $ parse template
        tree <- parseTypeTree usersSchema
        getStartingSuggestions' 15 ast tree `shouldEqual`
          { eaches: pure "friends"
          , vars: "id" : "name" : Nil
          }

getStartingSuggestions'
  :: Int
  -> List AstPos
  -> GqlTypeTree
  -> { eaches :: List String
     , vars :: List String
     }
getStartingSuggestions' idx ast tree = getStartingSuggestions idx ast tree # mapRecord (map _.field)

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

