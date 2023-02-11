module GraphQL.Templater.View.Gui (Input, component) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH

data Action
  = Init
  | Receive Input

type Input = {}

type State = {}

component :: forall output m q. H.Component q {} output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }
  where
  initialState :: Input -> State
  initialState = identity

  render :: State -> HH.HTML _ Action
  render state =
    HH.div_
      []

  handleAction :: Action -> H.HalogenM State Action _ _ m Unit
  handleAction = case _ of
    Init -> pure unit

    Receive input ->
      H.modify_ \_state -> input