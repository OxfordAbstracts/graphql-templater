module GraphQL.Templater.Eval (EvalResult(..), EvalError(..), eval) where

import Prelude

import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader)
import Affjax.ResponseFormat (json)
import Affjax.Web (defaultRequest)
import Affjax.Web as Affjax
import Control.Monad.State (class MonadState, get, modify_)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse_)
import Effect.Aff (Milliseconds, delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import GraphQL.Templater.Ast (Ast)
import GraphQL.Templater.Cache.FullQuery (FullQueryCache)
import GraphQL.Templater.Eval.Interpolate (interpolate)
import GraphQL.Templater.Eval.MakeQuery (toGqlString)

data EvalResult
  = EvalSuccess String
  | NoQuery
  | DidNotRun
  | EvalFailure EvalError

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

-- | Fetch the data for a template and interpolate the variables. 
eval
  :: forall a m st
   . MonadAff m
  => Ord a
  => MonadState
       { fullQueryCache :: FullQueryCache
       , mostRecentEval :: Maybe Instant
       | st
       }
       m
  => { url :: String
     , headers :: Array RequestHeader
     , ast :: List (Ast a)
     , debounceTime :: Milliseconds
     }
  -> m EvalResult
eval { url, headers, ast, debounceTime } = do
  start <- liftEffect now
  modify_ \st -> st { mostRecentEval = max (Just start) st.mostRecentEval }
  liftAff $ delay $ debounceTime
  whenMostRecent start do
    toGqlString ast # maybe (pure NoQuery) \query -> do
      { fullQueryCache } <- get
      res <- case Map.lookup query fullQueryCache of
        Nothing -> do
          json <- getJsonViaNetwork query
          addResultToCache query json
          pure json
        Just json -> pure $ Right json
      whenMostRecent start do
        case res of
          Left err -> pure $ EvalFailure err
          Right json -> pure $ EvalSuccess $ interpolate ast json

  where
  whenMostRecent start m = do
    { mostRecentEval } <- get
    if maybe false (_ > start) mostRecentEval then
      pure DidNotRun
    else
      m

  getJsonViaNetwork :: String -> m (Either EvalError Json)
  getJsonViaNetwork query = liftAff do
    res <- Affjax.request
      defaultRequest
        { method = Left POST
        , responseFormat = json
        , url = url
        , headers = headers
        , content = Just $ Json $ encodeJson
            { query
            }
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

  addResultToCache query = traverse_ \json ->
    modify_ \st -> st { fullQueryCache = Map.insert query json st.fullQueryCache }
