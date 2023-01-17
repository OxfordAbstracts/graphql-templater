module GraphQL.Templater.View.Editor
  ( EditorView(..)
  , Input
  , Output(..)
  , Query(..)
  , ViewUpdate(..)
  , component
  , getViewUpdateContent
  ) where

import Prelude

import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

type Input = { doc :: String }

data Query a =
  GetContent (Maybe String -> a)

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
      { input: { doc } } <- H.get
      elMb <- H.getRef label
      for_ elMb \parent -> do
        { emitter, listener } <- H.liftEffect HS.create
        void $ H.subscribe emitter
        view <- liftEffect $ makeView
          { parent
          , doc
          , onChange: HS.notify listener <<< HandleChange
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
      { view } <- H.get
      content <- liftEffect $ getViewContent $ unsafeCoerce view
      pure (Just (reply $ Just content))

  render :: State -> _
  render _st = HH.div [ HP.ref label ] []

  label = RefLabel "editor-parent"

foreign import makeView
  :: { parent :: Element
     , doc :: String
     , onChange :: ViewUpdate -> Effect Unit
     }
  -> Effect EditorView

foreign import getViewContent :: EditorView -> Effect String
foreign import getViewUpdateContent :: ViewUpdate -> Effect String

data EditorView

-- | https://codemirror.net/docs/ref/#view.ViewUpdate
data ViewUpdate