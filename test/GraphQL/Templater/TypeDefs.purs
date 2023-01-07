module Test.GraphQL.Templater.TypeDefs (spec) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.GraphQL.Parser (document)
import Data.List as List
import Data.Map.Internal as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error, error)
import GraphQL.Templater.TypeDefs (GqlTypeTree(..), getTypeAtPath, getTypeTreeFromDoc)
import Parsing (ParseError, parseErrorMessage, runParser)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

spec :: Spec Unit
spec = do
  describe "GraphQL.Templater.TypeDefs" do
    describe "getTypeTreeFromDoc & getTypeAtPath" do
      it "should return a nested type map for a simple schema" do
        doc <- parseDoc usersSchema

        let tree = fromMaybe GqlUndefined $ getTypeTreeFromDoc doc

        testTree tree [ "top_level" ] (Node "String")

        testTree tree [ "user", "id" ] (NonNull $ Node "ID")

        testTree tree [ "users", "friends", "name" ] (Node "String")

        testTree tree [ "user" ]
          ( ( ObjectType $ Map.fromFoldable
                [ Tuple "friends" stub
                , Tuple "id" stub
                , Tuple "name" stub
                ]
            )
          )

      it "should return a nested type map for a schema with unions" do
        doc <- parseDoc schemaWithUnions

        let tree = fromMaybe GqlUndefined $ getTypeTreeFromDoc doc

        testTree tree [ "media", "name" ] (Node "String")

        testTree tree [ "media", "title" ] (Node "String")

  where
  stub = unsafeCoerce unit

  parseDoc str = runParser str document # throwParser

  testTree tree path result =
    getTypeAtPathF path tree
      `shouldEqual`
        Just (showGqlTypeTree result)

  getTypeAtPathF path t = showGqlTypeTree <$> getTypeAtPath (List.fromFoldable path) t

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

  schemaWithUnions =
    """
    type Query {
      media: [Media]
    }

    type Book {
      name: String
    }

    type Movie {
      title: String
    }
  union Media = Book | Movie

  """

throwParser :: forall m a. MonadThrow Error m => Either ParseError a -> m a
throwParser = case _ of
  Left errs -> throwError $ error $ parseErrorMessage errs
  Right a -> pure a

showGqlTypeTree :: GqlTypeTree -> String
showGqlTypeTree = case _ of
  Node n -> "(Node " <> show n <> ")"
  ObjectType m -> "(ObjectType " <> show (Array.fromFoldable $ Map.keys m) <> ")"
  ListType t -> "(ListType " <> showGqlTypeTree t <> ")"
  NonNull t -> "(NonNull " <> showGqlTypeTree t <> ")"
  GqlUndefined -> "GqlUndefined"