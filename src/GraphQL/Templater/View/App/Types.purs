module GraphQL.Templater.View.App.Types where

import Prelude

import Data.Argonaut.Core (Json)
import Data.DateTime.Instant (Instant)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect.Ref (Ref)
import GraphQL.Templater.Ast (Ast, AstPos, VarPartName(..))
import GraphQL.Templater.TypeDefs (GqlTypeTree)
import GraphQL.Templater.View.Autocomplete (AutocompleteState)
import GraphQL.Templater.View.Component.Editor (ViewUpdate)
import Parsing (Position)

data Action
  = Init
  | SetUrl String
  | SetHeaders String
  | SetTemplate ViewUpdate
  | SetCursorPosition ViewUpdate
  | InsertVariable (Array (VarPartName Unit))
  | InsertEach (Array (VarPartName Unit))
  | InsertWith (Array (VarPartName Unit))
  | ModifyAstAt (AstPos -> List (Ast Unit)) Int

type State =
  { url :: String
  , headers :: String
  , template :: String
  , ast :: List AstPos
  , errors :: List TemplaterError
  , result :: String
  , printedSchema :: Maybe String
  , schemaTypeTree :: Maybe GqlTypeTree
  , fullQueryCache :: Map.Map String Json
  , mostRecentEval :: Maybe Instant
  , autocompleteState :: Maybe (Ref (Maybe AutocompleteState))
  , cursorPosition :: Maybe Int
  }

type TemplaterError =
  { message :: String
  , from :: Int
  , to :: Maybe Int
  }
