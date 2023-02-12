module GraphQL.Templater.View.App.Types where



import Data.Argonaut.Core (Json)
import Data.DateTime.Instant (Instant)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe)
import Effect.Ref (Ref)
import GraphQL.Templater.Ast (AstPos)
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
  | InsertVariable (Array String)
  | InsertEach (Array String)
  | InsertWith (Array String)

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
  , from :: Position
  , to :: Maybe Position
  }
