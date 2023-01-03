module GraphQL.Templater.GetSchema (getGqlDoc) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either(..))
import Data.GraphQL.AST (Document)
import Data.GraphQL.Parser (document)
import Data.Nullable (Nullable, null)
import Effect (Effect)
import Effect.Aff (Aff, Error, error, throwError)
import Foreign.Object (Object)
import Parsing (parseErrorMessage, runParser)

getGqlDoc :: String -> Object String -> Aff Document
getGqlDoc url headers = do
  schema <- Promise.toAffE $ getGqlSchemaImpl
    { url
    , token: null
    , headers
    }
  rethrow parseErrorMessage $ runParser schema document

rethrow :: forall err m a. MonadThrow Error m => (err -> String) -> Either err a -> m a
rethrow fn = case _ of
  Left err -> throwError $ error $ fn err
  Right schema -> pure schema

foreign import getGqlSchemaImpl
  :: { url :: String
     , token :: Nullable String
     , headers :: Object String
     }
  -> Effect (Promise String)
