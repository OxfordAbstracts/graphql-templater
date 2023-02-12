module GraphQL.Templater.View.Autocomplete (AutocompleteState(..), autocompletion) where

import Prelude

import Data.Array (uncons)
import Data.Array as Array
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Debug (spy, spyWith, traceM)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Effect.Ref (Ref)
import GraphQL.Templater.Ast (AstPos)
import GraphQL.Templater.Ast.Print (printPositioned)
import GraphQL.Templater.Ast.Transform (insertEmptyEachAt, insertSingleVarAt)
import GraphQL.Templater.TypeDefs (GqlTypeTree)
import GraphQL.Templater.View.Component.Editor (CompletionContext, CompletionResult, Match, matchBefore, setContent)
import GraphQL.TemplaterAst.Suggest (getStartingSuggestions)

data AutocompleteState
  = InVar
  | InEach
  | InWith

autocompletion
  :: forall m
   . MonadEffect m
  => Maybe (Ref (Maybe AutocompleteState))
  -> Maybe (List (AstPos))
  -> Maybe GqlTypeTree
  -> CompletionContext
  -> m (Maybe CompletionResult)
autocompletion stRef astsMb typeTreeMb ctx = firstMatch
  [ { rgx: unsafeRegex """\{\{#""" noFlags
    , result: getKeywords
    }
  , { rgx: unsafeRegex """\{\{\w*""" noFlags
    , result: getVars
    }
  ]
  where
  getKeywords { from } =
    pure $ Just
      { filter: false
      , from
      , options:
          [ { label: "each"
            , detail: Just "loop over a list"
            , info: Nothing
            , type: Just "keyword"
            , apply: Just \applyInput@{ view } -> do
                case astsMb of
                  Nothing -> pure unit
                  Just asts ->
                    case insertEmptyEachAt "list" applyInput.from asts of
                      Just asts' -> do
                        setContent (printPositioned asts') view
                      Nothing -> Console.error $ "Failed to insert each at index " <> show applyInput.from
            }
          ]
      }

  getVars { from } = do
    case astsMb, typeTreeMb of
      Just asts, Just typeTree -> do
        let { vars } = getStartingSuggestions (spy "from" from) (spyWith "template" printPositioned asts) typeTree
        traceM { vars }
        pure $ Just
          { filter: false
          , from
          , options: Array.fromFoldable vars <#> \{ field, description } ->
              { label: field
              , detail: Nothing
              , info: description
              , type: Just "variable"
              , apply: Just \applyInput@{ view } ->
                  case insertSingleVarAt field applyInput.from asts of
                    Just asts' -> setContent (printPositioned asts') view
                    _ -> Console.error $ "Failed to insert var at index " <> show applyInput.from
              }
          }
      _, _ -> pure Nothing

  firstMatch :: Array { rgx :: Regex, result :: Match -> m (Maybe CompletionResult) } -> m (Maybe CompletionResult)
  firstMatch inputs = case uncons inputs of
    Just { head: { rgx, result }, tail } -> do
      matchMb <- matchBefore rgx ctx
      case matchMb of
        Just match -> result match
        Nothing -> firstMatch tail
    Nothing -> pure Nothing