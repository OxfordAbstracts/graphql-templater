module GraphQL.Templater.View.Editor
  ( Diagnostic
  , EditorView(..)
  , Input
  , Output(..)
  , Query(..)
  , Severity(..)
  , ViewUpdate(..)
  , component
  , getViewUpdateContent
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Web.DOM (Element)

type Input =
  { doc :: String
  , lint :: Array Diagnostic
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
      { input: { doc, lint } } <- H.get
      elMb <- H.getRef label
      for_ elMb \parent -> do
        { emitter, listener } <- H.liftEffect HS.create
        void $ H.subscribe emitter
        view <- liftEffect $ makeView
          { parent
          , doc
          , onChange: HS.notify listener <<< HandleChange
          , lint: toForeignDiagnostic <$> lint
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
     }
  -> Effect EditorView

foreign import getViewContent :: EditorView -> Effect String
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