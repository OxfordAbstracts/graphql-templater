module GraphQL.Templater.View.App (component) where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Error.Class (try)
import Data.Argonaut.Core (Json)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..), either, hush)
import Data.Foldable (intercalate)
import Data.GraphQL.AST.Print (printAst)
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Debug (spy, traceM)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Effect.Exception (message)
import Foreign.Object as Object
import GraphQL.Templater.Ast (Ast(..), AstPos)
import GraphQL.Templater.Ast.Print (printPositioned)
import GraphQL.Templater.Ast.Transform (insertTextAt)
import GraphQL.Templater.Eval (EvalResult(..), eval)
import GraphQL.Templater.Eval.MakeQuery (toGqlString)
import GraphQL.Templater.GetSchema (getGqlDoc)
import GraphQL.Templater.Parser (parse)
import GraphQL.Templater.TypeCheck (getTypeErrorsFromTree)
import GraphQL.Templater.TypeCheck.Errors (TypeErrorWithPath(..))
import GraphQL.Templater.TypeCheck.Errors.Display (displayPositionedError)
import GraphQL.Templater.TypeCheck.Errors.GetPositions (getPositions)
import GraphQL.Templater.TypeDefs (GqlTypeTree, getTypeTreeFromDoc)
import GraphQL.Templater.View.Editor (Diagnostic, ViewUpdate, getViewContent, getViewUpdateContent, matchBefore, setContent)
import GraphQL.Templater.View.Editor as Editor
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Parsing (ParseError(..), Position(..), initialPos)
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
  , ast :: Maybe (List AstPos)
  , errorDiagnostics :: Array Diagnostic
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
    , errorDiagnostics: []
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
      , HH.slot (Proxy :: Proxy "Editor") unit Editor.component
          { doc: initialQuery
          , lint: state.errorDiagnostics
          , autocompletion: Just \ctx -> do
              matchBrackets <- matchBefore (unsafeRegex """\{\{\w*""" noFlags) ctx
              for matchBrackets \{ text, from } -> do
                pure
                  { filter: false
                  , from
                  , options:
                      [ { label: "each"
                        , detail: Just "loop over a list"
                        , info: Nothing
                        , type: Just "keyword"
                        , apply: Just \applyInput@{ view } -> do
                            content <- getViewContent view
                            case parse content of
                              Left _ -> pure unit
                              Right asts' ->
                                case insertTextAt "each" applyInput.from asts' of
                                  Just asts'' -> do
                                    setContent (printPositioned asts'') view
                                  _ -> Console.error $ "Failed to insert each at index " <> show applyInput.from
                            pure unit
                        }
                      ]
                  }
          }
          case _ of
            Editor.DocChanged viewUpdate -> SetTemplate viewUpdate

      , HH.div [] [ HH.text "Result:" ]
      , HH.pre
          [ HP.ref resultLabel
          , css "border-2 rounded-md p-1 m-2 whitespace-pre-wrap"
          ]
          [ HH.text state.result ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2 whitespace-pre" ]
          [ HH.text $ (toGqlString >>> fromMaybe "No query") asts
          ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2" ]
          [ HH.text $ (map (map (const unit) >>> show) >>> intercalate "\n") asts
          ]

      , HH.pre
          [ css "border-2 rounded-md p-1 m-2" ]
          [ HH.text $ case state.printedSchema of
              Just doc -> String.take 2000 doc
              Nothing -> "Loading schema"
          ]
      ]
    where
    asts :: List AstPos
    asts = fromMaybe (Nil) state.ast

  handleAction = case _ of
    Init -> loadSchema
    SetUrl url -> do
      H.modify_ _ { url = url }
      loadSchema
    SetHeaders headers -> do
      H.modify_ _ { headers = headers }
    SetTemplate viewUpdate -> do
      template <- liftEffect $ getViewUpdateContent viewUpdate
      { url, headers, schemaTypeTree } <- H.modify _ { template = template }
      case parse template of
        Left (ParseError err (Position pos)) -> H.modify_ _
          { errorDiagnostics = pure
              { from: pos.index
              , message: err
              , severity: Editor.Error
              , to: pos.index + 1
              }
          }
        Right ast -> do
          let typeErrors = maybe Nil (flip getTypeErrorsFromTree ast) schemaTypeTree
          { errorDiagnostics } <- H.modify _
            { errorDiagnostics = Array.fromFoldable $ typeErrors
                <#> \err@(TypeErrorWithPath _ _path _) ->
                  let
                    { start: Position start, end: Position end } = getPositions err
                  in
                    { from: start.index
                    , message: displayPositionedError err
                    , severity: Editor.Error
                    , to: end.index
                    }
            }

          H.tell _editor unit $ Editor.Relint errorDiagnostics

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
            EvalFailure err -> H.modify_ _
              { result = show err
              , ast = Just ast
              }
            EvalSuccess resultStr -> H.modify_ _ { result = resultStr, ast = Just ast }

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

_editor = Proxy :: Proxy "Editor"

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