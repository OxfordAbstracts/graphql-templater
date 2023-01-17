module GraphQL.Templater.View.App (component) where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Error.Class (try)
import Data.Argonaut.Core (Json)
import Data.Array (mapMaybe)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..), either, hush)
import Data.Foldable (intercalate)
import Data.GraphQL.AST.Print (printAst)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (message)
import Foreign.Object as Object
import GraphQL.Templater.Ast (AstPos)
import GraphQL.Templater.Eval (EvalResult(..), eval)
import GraphQL.Templater.Eval.MakeQuery (toGqlString)
import GraphQL.Templater.GetSchema (getGqlDoc)
import GraphQL.Templater.Parser (parse)
import GraphQL.Templater.TypeCheck (getTypeErrorsFromTree)
import GraphQL.Templater.TypeDefs (GqlTypeTree, getTypeTreeFromDoc)
import GraphQL.Templater.View.Editor (ViewUpdate, getViewUpdateContent)
import GraphQL.Templater.View.Editor as Editor
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Parsing (ParseError, parseErrorMessage)
import Type.Proxy (Proxy(..))

data Action
  = Init
  | SetUrl String
  | SetHeaders String
  | SetTemplate ViewUpdate

type State =
  { url :: String
  , headers :: String
  , template :: String
  , ast :: Maybe (Either ParseError (List AstPos))
  , result :: String
  , printedSchema :: Maybe String
  , schemaTypeTree :: Maybe GqlTypeTree
  , fullQueryCache :: Map.Map String Json
  , mostRecentEval :: Maybe Instant
  }

component :: forall output m q input. MonadAff m => H.Component q input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }
  where

  initialState :: _ -> State
  initialState _ =
    { url: initialUrl
    , headers: initialHeaders
    , template: initialQuery
    , ast: Nothing
    , result: ""
    , printedSchema: Nothing
    , schemaTypeTree: Nothing
    , fullQueryCache: Map.empty
    , mostRecentEval: Nothing
    }

  render :: State -> _
  render state =
    HH.div [ css "flex flex-col min-w-[32rem]" ]
      [ HH.input
          [ css "border-2 rounded-md p-1 m-2 "
          , HE.onValueInput SetUrl
          , HP.value state.url
          , HP.placeholder "Enter graphql endpoint URL"
          ]

      , HH.textarea
          [ HP.value state.headers
          , HP.placeholder "Enter headers"
          , HE.onValueInput SetHeaders
          , css "font-mono border-2 rounded-md p-1 m-2 h-12 whitespace-pre-wrap"
          ]
      , HH.slot (Proxy :: Proxy "Editor") unit Editor.component { doc: initialQuery } case _ of
          Editor.DocChanged viewUpdate -> SetTemplate viewUpdate

      , HH.div []
          case asts, state.schemaTypeTree of
            Right Nil, _ -> []
            Right asts', Just typeTree ->
              [ HH.div [] [ HH.text "Errors:" ]
              , HH.pre
                  [ HP.ref resultLabel
                  , css "border-2 rounded-md p-1 m-2 whitespace-pre-wrap"
                  ]
                  [ HH.text $ show $ getTypeErrorsFromTree typeTree asts' ]
              ]
            _, _ -> []

      , HH.div [] [ HH.text "Result:" ]
      , HH.pre
          [ HP.ref resultLabel
          , css "border-2 rounded-md p-1 m-2 whitespace-pre-wrap"
          ]
          [ HH.text state.result ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2 whitespace-pre" ]
          [ HH.text $ either parseErrorMessage (toGqlString >>> fromMaybe "No query") asts
          ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2" ]
          [ HH.text $ either parseErrorMessage (map (map (const unit) >>> show) >>> intercalate "\n") asts
          ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2" ]
          [ HH.text $ case state.printedSchema of
              Just doc -> String.take 2000 doc
              Nothing -> "Loading schema"
          ]
      ]
    where
    asts = fromMaybe (pure Nil) state.ast

  handleAction = case _ of
    Init -> loadSchema
    SetUrl url -> do
      H.modify_ _ { url = url }
      loadSchema
    SetHeaders headers -> do
      H.modify_ _ { headers = headers }
    SetTemplate viewUpdate -> do
      template <- liftEffect $ getViewUpdateContent viewUpdate
      { url, headers } <- H.modify _ { template = template }
      case parse template of
        Left _ -> pure unit
        Right ast -> do
          res <- eval
            { ast
            , url
            , headers: headers # split (Pattern "\n") # mapMaybe \str -> case split (Pattern ":") str of
                [ key, value ] -> Just $ RequestHeader key value
                _ -> Nothing
            , debounceTime: Milliseconds 250.0
            }
          case res of
            NoQuery -> pure unit
            DidNotRun -> pure unit
            EvalFailure err -> H.modify_ _ { result = show err, ast = Just $ Right ast }
            EvalSuccess resultStr -> H.modify_ _ { result = resultStr, ast = Just $ Right ast }

    where
    loadSchema = do
      st <- H.get
      doc <- H.liftAff $ try $ getGqlDoc st.url $ Object.fromFoldable
        $ st.headers
            # split (Pattern "\n")
            # mapMaybe \str -> case split (Pattern ":") str of
                [ key, value ] -> Just $ Tuple key value
                _ -> Nothing
      H.modify_ _
        { printedSchema = Just $ either
            ( \err ->
                "Failed to load schema: " <> message err
            )
            printAst
            doc
        , schemaTypeTree = getTypeTreeFromDoc =<< hush doc
        }

  resultLabel = H.RefLabel "result"

-- where 

css
  :: forall r a
   . String
  -> IProp
       ( class :: String
       | r
       )
       a
css = class_ <<< ClassName

foreign import initialUrl :: String
foreign import initialHeaders :: String
foreign import initialQuery :: String