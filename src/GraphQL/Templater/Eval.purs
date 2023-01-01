module GraphQL.Templater.Eval where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web as Affjax
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Debug (spy)
import Effect.Aff (Aff)
import GraphQL.Templater.Ast (Ast)
import GraphQL.Templater.Eval.Interpolate (interpolate)
import GraphQL.Templater.Eval.MakeQuery (toGqlString)

data EvalError
  = GqlErrors (NonEmptyArray { message :: String })
  | DecodeError JsonDecodeError
  | RequestError Affjax.Error

derive instance Generic EvalError _

instance Show EvalError where
  show = case _ of
    GqlErrors errs -> "(GqlErrors " <> show errs <> ")"
    DecodeError err -> "(DecodeError " <> show err <> ")"
    RequestError err -> "(RequestError " <> Affjax.printError err <> ")"

eval :: forall a. { url :: String, ast :: List (Ast a) } -> Aff (Either EvalError String)
eval { url, ast } = case toGqlString ast of
  Nothing -> pure $ Right "No Query"
  Just query -> do
    res <- Affjax.post json url $ Just $ Json $ encodeJson
      { query
      }
    case res of
      Left err -> pure $ Left $ RequestError err
      Right { body } -> do
        case decodeJson body of
          Left err -> pure $ Left $ DecodeError err
          Right (gqlRes :: { data :: Json, errors :: Maybe (NonEmptyArray { message :: String }) }) -> do
            case gqlRes.errors of
              Just errs -> pure $ Left $ GqlErrors errs
              Nothing -> do
                pure $ Right $ interpolate ast gqlRes.data
