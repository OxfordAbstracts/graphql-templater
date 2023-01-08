module GraphQL.Templater.GetSchema (getGqlDoc) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either(..))
import Data.GraphQL.AST (Document)
import Data.GraphQL.Parser (document)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, error, throwError)
import Foreign.Object (Object)
import GraphQL.Templater.Cache.Schema (getCachedSchema, setCachedSchema)
import Parsing (parseErrorMessage, runParser)

getGqlDoc :: String -> Object String -> Aff Document
getGqlDoc url headers = do
  cacheMb <- getCachedSchema { url, headers }
  case cacheMb of
    Just schema -> pure schema
    Nothing -> do
      schema <- Promise.toAffE $ getGqlSchemaImpl
        { url
        , headers
        }
      res <- rethrow parseErrorMessage $ runParser schema document
      setCachedSchema { url, headers } res
      pure res

rethrow :: forall err m a. MonadThrow Error m => (err -> String) -> Either err a -> m a
rethrow fn = case _ of
  Left err -> throwError $ error $ fn err
  Right schema -> pure schema

foreign import getGqlSchemaImpl
  :: { url :: String
     , headers :: Object String
     }
  -> Effect (Promise String)
