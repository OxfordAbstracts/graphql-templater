module GraphQL.Templater.View.App (component) where

import Prelude

import Affjax.RequestHeader (RequestHeader(..))
import Control.Monad.Error.Class (try)
import Data.Array (mapMaybe)
import Data.Array as Array
import Data.Array.NonEmpty (toUnfoldable1)
import Data.Array.NonEmpty as NonEmpty
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), either, hush)
import Data.Foldable (intercalate)
import Data.GraphQL.AST.Print (printAst)
import Data.List (List(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), joinWith, split)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console as Console
import Effect.Exception (message)
import Effect.Ref as Ref
import Foreign.Object as Object
import GraphQL.Templater.Ast (AstPos, VarPathPart(..))
import GraphQL.Templater.Ast.Parser (parse)
import GraphQL.Templater.Ast.Print (printPositioned)
import GraphQL.Templater.Ast.Transform (insertEmptyEachAt', insertEmptyWithAt', insertVarAt', modifyAstStartingAt)
import GraphQL.Templater.Ast.TypeCheck (getTypeErrorsFromTree)
import GraphQL.Templater.Ast.TypeCheck.Errors (TypeErrorWithPath(..))
import GraphQL.Templater.Ast.TypeCheck.Errors.Display (displayPositionedError)
import GraphQL.Templater.Ast.TypeCheck.Errors.GetPositions (getPositions)
import GraphQL.Templater.Eval (EvalResult(..), eval)
import GraphQL.Templater.Eval.MakeQuery (toGqlString)
import GraphQL.Templater.GetSchema (getGqlDoc)
import GraphQL.Templater.TypeDefs (getTypeTreeFromDoc)
import GraphQL.Templater.View.App.Gui (gui)
import GraphQL.Templater.View.App.Types (Action(..), State, TemplaterError)
import GraphQL.Templater.View.Component.Editor (Diagnostic, Query(..), getViewUpdateContent, getViewUpdateSelectionRanges)
import GraphQL.Templater.View.Component.Editor as Editor
import GraphQL.Templater.View.Html.Utils (css)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Parsing (ParseError(..), Position(..))
import Parsing.String (parseErrorHuman)
import Type.Proxy (Proxy(..))

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
    , ast: Nil
    , result: ""
    , errors: Nil
    , printedSchema: Nothing
    , schemaTypeTree: Nothing
    , fullQueryCache: Map.empty
    , mostRecentEval: Nothing
    , autocompleteState: Nothing
    , cursorPosition: Nothing
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
      , HH.div [ css "flex" ]
          [ gui state
          , HH.div
              []
              [ HH.slot (Proxy :: Proxy "Editor") unit Editor.component
                  { doc: initialQuery
                  , lint: []
                  , autocompletion: Nothing -- Just $ autocompletion state.autocompleteState state.ast state.schemaTypeTree
                  }
                  case _ of
                    Editor.DocChanged viewUpdate -> SetTemplate viewUpdate
                    Editor.SelectionChanged viewUpdate -> SetCursorPosition viewUpdate
              ]
          ]

      , if state.errors /= Nil then
          HH.div [ css "p-1 m-2 bg-red-200 text-red-800" ]
            [ HH.text "Errors:"
            , HH.ul_ $ Array.fromFoldable state.errors <#> \{ from, message } ->
                HH.li [ css "whitespace-pre-wrap border-2 rounded-md p-1 m-2" ]
                  [ HH.text $ joinWith "\n" $ parseErrorHuman state.template 64 $ ParseError message (getPositionAt state.template from) ]
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
    asts = state.ast

  handleAction = case _ of
    Init -> do
      autocompleteState <- liftEffect $ Ref.new Nothing
      st <- H.modify _ { autocompleteState = Just autocompleteState }
      loadSchema
      handleNewTemplate st.template
    SetUrl url -> do
      H.modify_ _ { url = url }
      loadSchema
    SetHeaders headers -> do
      H.modify_ _ { headers = headers }
    SetCursorPosition viewUpdate -> setCursorPosition viewUpdate
    SetTemplate viewUpdate -> do
      setCursorPosition viewUpdate
      template <- liftEffect $ getViewUpdateContent viewUpdate
      handleNewTemplate template
    InsertVariable path -> updateAtPath insertVarAt' (nameToPart path)
    InsertEach path -> updateAtPath insertEmptyEachAt' (nameToPart path)
    InsertWith path -> updateAtPath insertEmptyWithAt' (nameToPart path)

    ModifyAstAt fn idx -> do
      { ast } <- H.get
      void $ handleNewAst $ modifyAstStartingAt fn idx ast
      H.tell _editor unit $ SetSelection
        { anchor: idx + 2
        , head: idx + 2
        }
      updateResult

    where
    nameToPart = map \name -> VarPathPart { name, args: Nothing } unit

    updateAtPath fn path = do
      { cursorPosition, template, ast } <- H.get
      case NonEmptyArray.fromArray path of
        Nothing -> pure unit
        Just path' -> do

          let
            pos = fromMaybe (String.length template) cursorPosition
            newAstMb = fn (toUnfoldable1 path') pos ast
          case newAstMb of
            Nothing -> Console.error $ "Failed update with path " <> show path <> " at position " <> show cursorPosition <> "."
            Just newAst -> do
              newTemplate <- handleNewAst newAst
              H.tell _editor unit $ SetSelection
                { anchor: pos + 2
                , head: pos + String.length newTemplate - String.length template - 2
                }
              updateResult

    setCursorPosition viewUpdate = do
      ranges <- liftEffect $ getViewUpdateSelectionRanges viewUpdate
      H.modify_ _ { cursorPosition = Just (NonEmpty.head ranges).from }
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

    handleNewAst ast = do
      let template = printPositioned ast
      H.modify_ _
        { ast = ast
        , template = template
        }

      H.tell _editor unit $ Editor.SetContent template

      pure template

    handleNewTemplate template = do

      { schemaTypeTree } <- H.modify _ { template = template }
      let

        toDiagnostic :: TemplaterError -> Diagnostic
        toDiagnostic err =
          let
            getIndex index = index
            from = getIndex err.from
          in
            { from
            , message: err.message
            , severity: Editor.Error
            , to: maybe (from + 1) getIndex err.to
            }

        relint errs = pure unit --  H.tell _editor unit $ Editor.Relint $ Array.fromFoldable $ map toDiagnostic errs

      case parse template of
        Left (ParseError message (Position {index: pos})) -> do
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
            , ast = ast
            }
          relint errors
          updateResult

    updateResult = do
      { url, headers, ast } <- H.get
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
          }
        EvalSuccess resultStr -> H.modify_ _ { result = resultStr }

  resultLabel = H.RefLabel "result"

_editor = Proxy :: Proxy "Editor"

-- where 

foreign import initialUrl :: String
foreign import initialHeaders :: String
foreign import initialQuery :: String


getPositionAt :: String -> Int -> Position
getPositionAt str index =
  let
    {before, after} = String.splitAt index str
    count p s = split p s # Array.length
  in
   Position
    { index
    , line: count (Pattern "\n") before
    , column: String.length before - count (Pattern "\n") before
    }