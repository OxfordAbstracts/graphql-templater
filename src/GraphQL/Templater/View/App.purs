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
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.String as String
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Effect.Exception (message)
import Foreign.Object as Object
import GraphQL.Templater.Ast (AstPos)
import GraphQL.Templater.Ast.Parser (parse)
import GraphQL.Templater.Ast.Print (printPositioned)
import GraphQL.Templater.Ast.Transform (insertEmptyEachAt, insertTextAt)
import GraphQL.Templater.Eval (EvalResult(..), eval)
import GraphQL.Templater.Eval.MakeQuery (toGqlString)
import GraphQL.Templater.GetSchema (getGqlDoc)
import GraphQL.Templater.TypeCheck (getTypeErrorsFromTree)
import GraphQL.Templater.TypeCheck.Errors (TypeErrorWithPath(..))
import GraphQL.Templater.TypeCheck.Errors.Display (displayPositionedError)
import GraphQL.Templater.TypeCheck.Errors.GetPositions (getPositions)
import GraphQL.Templater.TypeDefs (GqlTypeTree, getTypeTreeFromDoc)
import GraphQL.Templater.View.Editor (ViewUpdate, Diagnostic, getViewContent, getViewUpdateContent, matchBefore, setContent)
import GraphQL.Templater.View.Editor as Editor
import Halogen (ClassName(..), liftEffect)
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (class_)
import Halogen.HTML.Properties as HP
import Parsing (ParseError(..), Position(..))
import Parsing.String (parseErrorHuman)
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
  , errors :: List TemplaterError
  , result :: String
  , printedSchema :: Maybe String
  , schemaTypeTree :: Maybe GqlTypeTree
  , fullQueryCache :: Map.Map String Json
  , mostRecentEval :: Maybe Instant
  }

type TemplaterError =
  { message :: String
  , from :: Position
  , to :: Maybe Position
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
    , errors: Nil
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
          , lint: [] -- state.errorDiagnostics
          , autocompletion: Just \ctx -> do
              matchBrackets <- matchBefore (unsafeRegex """\{\{\w*""" noFlags) ctx
              for matchBrackets \{ from } -> do
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
                              Left err -> Console.error $ joinWith "\n" (parseErrorHuman content 64 err)
                              Right asts' ->
                                let
                                  path = "list"
                                in
                                  case insertEmptyEachAt path applyInput.from asts' of
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

      , if state.errors /= Nil then
          HH.div [ css "p-1 m-2 bg-red-200 text-red-800" ]
            [ HH.text "Errors:"
            , HH.ul_ $ Array.fromFoldable state.errors <#> \{ from, message } ->
                HH.li [ css "whitespace-pre-wrap border-2 rounded-md p-1 m-2" ]
                  [ HH.text $ joinWith "\n" $ parseErrorHuman state.template 64 $ ParseError message from ]
            ]
        else
          HH.text ""

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
      let

        toDiagnostic :: TemplaterError -> Diagnostic
        toDiagnostic err =
          let
            getIndex (Position { index }) = index
            from = getIndex err.from
          in
            { from
            , message: err.message
            , severity: Editor.Error
            , to: maybe (from + 1) getIndex err.to
            }

        relint errs = H.tell _editor unit $ Editor.Relint $ Array.fromFoldable $ map toDiagnostic errs

      case parse template of
        Left (ParseError message pos) -> do
          { errors } <- H.modify _
            { errors = pure
                { from: pos
                , message
                , to: Nothing
                }
            }
          relint errors
        Right ast -> do
          let typeErrors = maybe Nil (flip getTypeErrorsFromTree ast) schemaTypeTree
          { errors } <- H.modify _
            { errors = typeErrors
                <#> \err@(TypeErrorWithPath _ _path _) ->
                  let
                    { start, end } = getPositions err
                  in
                    { from: start
                    , message: displayPositionedError err
                    , to: Just end
                    }
            }

          relint errors

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