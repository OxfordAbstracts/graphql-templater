module GraphQL.Templater.Cache.Schema
  ( CacheInput
  , getCachedSchema
  , setCachedSchema
  ) where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, parseJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (hush)
import Data.GraphQL.AST (Document)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Foreign.Object (Object)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

type CacheInput = { url :: String, headers :: Object String }

getCachedSchema :: forall m. MonadAff m => CacheInput -> m (Maybe Document)
getCachedSchema opts = do
  compressedMb <- liftEffect $ window >>= localStorage >>= getItem (getKey opts)
  case compressedMb of
    Nothing -> pure Nothing
    Just compressed -> do
      liftAff $ pure $ decode $ decompress compressed
  where
  decode = (parseJson >=> decodeJson) >>> hush

setCachedSchema :: forall m. MonadAff m => CacheInput -> Document -> m Unit
setCachedSchema opts schema = do
  let compressed = compress $ encode schema
  liftEffect $
    window >>= localStorage >>= setItem (getKey opts) compressed

getKey :: CacheInput -> String
getKey opts = "schema-cache-" <> encode opts

encode :: forall a. EncodeJson a => a -> String
encode = stringify <<< encodeJson

foreign import compress :: String -> String
foreign import decompress :: String -> String