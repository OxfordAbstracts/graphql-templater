module GraphQL.Templater.View.Editor
  ( Completion(..)
  , CompletionContext(..)
  , CompletionResult
  , CompletionSource
  , Diagnostic
  , EditorView(..)
  , Input
  , Match
  , Output(..)
  , Query(..)
  , Severity(..)
  , ViewUpdate(..)
  , component
  , explicit
  , getViewContent
  , getViewUpdateContent
  , matchBefore
  , setContent
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Foldable (for_)
import Data.Function.Uncurried (Fn4, mkFn4)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull, null, toMaybe, toNullable)
import Data.String.Regex (Regex)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.DOM (Element)

type Input =
  { doc :: String
  , lint :: Array Diagnostic
  , autocompletion :: Maybe CompletionSource
  }

data Query a
  = GetContent (Maybe String -> a)
  | Relint (Array Diagnostic) a

type State =
  { input :: Input
  , view :: Maybe EditorView
  }

data Action
  = Init
  | HandleChange ViewUpdate

data Output = DocChanged ViewUpdate

component :: forall m. MonadAff m => H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Init
        }
    }
  where
  initialState :: Input -> State
  initialState =
    { input: _
    , view: Nothing
    }

  handleAction :: Action -> H.HalogenM _ _ _ _ m Unit
  handleAction = case _ of
    Init -> do
      { input: input@{ doc, lint } } <- H.get
      elMb <- H.getRef label
      for_ elMb \parent -> do
        { emitter, listener } <- H.liftEffect HS.create
        void $ H.subscribe emitter
        view <- liftEffect $ makeView
          { parent
          , doc
          , onChange: HS.notify listener <<< HandleChange
          , lint: toForeignDiagnostic <$> lint
          , autocomplete: toNullable $ completionSourceToForeign <$> input.autocompletion
          }
        H.modify_ _
          { view = Just view
          }

    HandleChange viewUpdate -> H.raise $ DocChanged viewUpdate

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM _ _ _ _ m (Maybe a)
  handleQuery = case _ of
    GetContent reply -> do
      { view: viewMb } <- H.get
      case viewMb of
        Nothing -> pure $ Just $ reply Nothing
        Just view -> do
          content <- liftEffect $ getViewContent view
          pure $ Just $ reply $ Just content

    Relint lint reply -> do
      { view } <- H.get
      for_ view (relint lint)
      pure $ Just reply

  render :: State -> _
  render _st = HH.div [ HP.ref label ] []

  label = RefLabel "editor-parent"

foreign import makeView
  :: { parent :: Element
     , doc :: String
     , onChange :: ViewUpdate -> Effect Unit
     , lint :: Array DiagnosticForeign
     , autocomplete :: Nullable CompletionSourceForeign
     }
  -> Effect EditorView

foreign import getViewContent :: EditorView -> Effect String

foreign import setContent :: String -> EditorView -> Effect Unit

foreign import getViewUpdateContent :: ViewUpdate -> Effect String

relint :: forall m. MonadEffect m => Array Diagnostic -> EditorView -> m Unit
relint lint view = liftEffect $ relintImpl { view, lint: toForeignDiagnostic <$> lint }

foreign import relintImpl
  :: { view :: EditorView
     , lint :: Array DiagnosticForeign
     }
  -> Effect Unit

data EditorView

-- | https://codemirror.net/docs/ref/#view.ViewUpdate
data ViewUpdate

type Diagnostic =
  { from :: Int
  , to :: Int
  , severity :: Severity
  , message :: String
  }

data Severity = Error | Warning | Info

type DiagnosticForeign =
  { from :: Int
  , to :: Int
  , severity :: String
  , message :: String

  }

toForeignDiagnostic :: Diagnostic -> DiagnosticForeign
toForeignDiagnostic d = d
  { severity = case d.severity of
      Error -> "error"
      Warning -> "warning"
      Info -> "info"
  }

-- Autocompletion

type CompletionSource = CompletionContext -> Aff (Maybe CompletionResult)

completionSourceToForeign :: CompletionSource -> CompletionSourceForeign
completionSourceToForeign = map (map (map completionResultToForeign >>> toNullable) >>> fromAff)
  where
  completionResultToForeign :: CompletionResult -> CompletionResultForeign
  completionResultToForeign r = r
    { options = completionToForeign <$> r.options
    }

  completionToForeign :: Completion -> CompletionForeign
  completionToForeign c = CompletionForeign
    c
      { detail = toNullable c.detail
      , info = toNullable c.info
      , type = toNullable c.type
      , apply = case c.apply of
          Nothing -> null
          Just fn -> notNull $ mkFn4 \view _completion from to ->
            unsafePerformEffect $ fn { view, from, to }
      }

type CompletionSourceForeign = CompletionContext -> Effect (Promise (Nullable CompletionResultForeign))

data CompletionContext

-- | https://codemirror.net/docs/ref/#autocomplete.CompletionContext.matchBefore
matchBefore
  :: forall m
   . MonadEffect m
  => Regex
  -> CompletionContext
  -> m
       ( Maybe
           { from :: Int
           , text :: String
           , to :: Int
           }
       )
matchBefore rgx ctx = liftEffect $ toMaybe <$> matchBeforeImpl rgx ctx

foreign import matchBeforeImpl :: Regex -> CompletionContext -> Effect (Nullable Match)
foreign import explicit :: CompletionContext -> Boolean

type Match =
  { text :: String
  , from :: Int
  , to :: Int
  }

-- | https://codemirror.net/docs/ref/#autocomplete.CompletionResult
type CompletionResult =
  { from :: Int
  , filter :: Boolean
  , options :: Array Completion
  }

type CompletionResultForeign =
  { from :: Int
  , filter :: Boolean
  , options :: Array CompletionForeign
  }

-- | https://codemirror.net/docs/ref/#autocomplete.Completion
type Completion =
  { label :: String
  , detail :: Maybe String
  , info :: Maybe String
  , type :: Maybe String
  , apply ::
      Maybe
        ( { view :: EditorView
          , from :: Int
          , to :: Int
          }
          -> Effect Unit
        )
  }

newtype CompletionForeign = CompletionForeign
  { label :: String
  , detail :: Nullable String
  , info :: Nullable String
  , type :: Nullable String
  , apply :: Nullable (Fn4 EditorView CompletionForeign Int Int Unit)
  }
