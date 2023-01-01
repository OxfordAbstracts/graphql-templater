module GraphQL.Templater.Eval where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (json)
import Affjax.Web as Affjax
import Control.Monad.State (class MonadState, get, modify_)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (for, traverse_)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import GraphQL.Templater.Ast (Ast)
import GraphQL.Templater.Cache.FullQuery (FullQueryCache)
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

eval
  :: forall a m st
   . MonadAff m
  => MonadState { fullQueryCache :: FullQueryCache | st } m
  => { url :: String
     , ast :: List (Ast a)
     }
  -> m (Either EvalError String)
eval { url, ast } = case toGqlString ast of
  Nothing -> pure $ Right "No Query"
  Just query -> do
    {fullQueryCache} <- get
    res <- case Map.lookup query fullQueryCache of
      Nothing -> do
        json <- getJsonViaNetwork
        addResultToCache json
        pure json
      Just json -> pure $ Right json

    for res \json ->
      pure $ interpolate ast json

    where
    getJsonViaNetwork :: m (Either EvalError Json)
    getJsonViaNetwork = liftAff do
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
                Nothing -> pure $ Right gqlRes.data


    addResultToCache = traverse_ \json -> 
      modify_ \st -> st { fullQueryCache = Map.insert query json st.fullQueryCache }

