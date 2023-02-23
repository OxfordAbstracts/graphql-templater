module GraphQL.Templater.View.Component.NestedDropdown
  ( DropdownItem(..)
  , Input
  , nestedDropdown
  ) where

import Prelude

import Data.Array (filter, findMap, head, sortWith, uncons)
import Data.Lazy (Lazy, force)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.String (Pattern(..), contains)
import Data.String as String
import Data.String.Utils (startsWith)
import Effect.Class (class MonadEffect, liftEffect)
import GraphQL.Templater.View.Html.Icons (chevronDown, chevronRight, chevronUp)
import GraphQL.Templater.View.Html.Input (input)
import GraphQL.Templater.View.Html.Utils (css, whenElem)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.Event.Event (stopPropagation)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

data Action id
  = Init
  | Receive (Input id)
  | ToggleOpen
  | SetPath (Array id) MouseEvent
  | Select (Array id)
  | SetSearch (Array id) String

type Input id =
  { label :: String
  , path :: Array id
  , items :: Lazy (Array (DropdownItem id))
  }

data DropdownItem id
  = Node
      { label :: String
      , id :: id
      }
  | Parent
      { label :: String
      , children :: Lazy (Array (DropdownItem id))
      , id :: id
      , selectable :: Boolean
      }

type State id =
  { input :: Input id
  , open :: Boolean
  , path :: Array id
  , searches :: Map (Array id) String
  }

nestedDropdown
  :: forall id m q
   . MonadEffect m
  => Ord id
  => Show id
  => H.Component q (Input id) (Array id) m
nestedDropdown =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input id -> State id
  initialState input =
    { input
    , open: false
    , path: input.path
    , searches: Map.empty
    }

  render :: State id -> HH.HTML _ (Action id)
  render state@{ input: { items } } =
    HH.div [ css "relative mt-1" ]
      [ HH.button
          [ css "relative w-full cursor-pointer rounded-md border border-gray-300 bg-white py-2 pl-3 pr-10 text-left shadow-sm focus:border-blue-500 focus:outline-none focus:ring-1 focus:ring-blue-500 sm:text-sm"
          , HE.onClick \_ -> ToggleOpen
          ]
          [ HH.text state.input.label
          , HH.span
              [ css "pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2" ]
              [ if state.open then chevronUp [] else chevronDown []
              ]
          ]
      , whenElem state.open \_ ->
          HH.div [ css "absolute flex z-10 mt-1 max-h-[32rem] w-auto" ]
            $ renderItems state [] state.path
            $ force items
      ]

  renderItems :: State id -> Array id -> Array id -> Array (DropdownItem id) -> Array (HH.HTML _ (Action id))
  renderItems state currentPath pathToFollow items =
    [ HH.ul
        [ css "overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm" ]
        $
          searchHtml <>
            itemsHtml
    ]
      <> case childrenMay of
        Just { children, id, tail } ->
          renderItems state (currentPath <> [ id ]) tail (force children)
        _ -> []

    where
    searchHtml =
      [ input
          { label: "Search"
          , placeholder: ""
          , value: fromMaybe "" $ Map.lookup currentPath state.searches
          , onInput: SetSearch currentPath
          }
      ]

    currentSearch = fromMaybe "" $ Map.lookup currentPath state.searches

    hasStartMatch item =
      startsWith currentSearch (getLabel item)

    highlightSearch str = case String.indexOf (Pattern currentSearch) str of
      Nothing -> HH.text str
      Just idx ->
        let
          { before, after } = String.splitAt idx str
        in
          HH.span []
            [ HH.text before
            , HH.span [ css "bg-blue-800 text-gray-100" ] [ HH.text currentSearch ]
            , HH.text $ String.drop (String.length currentSearch) after
            ]

    itemsHtml =
      items
        # filter (contains (Pattern currentSearch) <<< getLabel)
        # sortWith (not hasStartMatch)
        <#> \item ->
          let
            itemCss selectable selected =
              "text-gray-900 relative flex w-full justify-between select-none py-2 pl-3 pr-3 "
                <> guard selectable " cursor-pointer font-bold"
                <> (if selected then " bg-blue-200" else " hover:bg-blue-100")

          in
            case item of
              Node { label, id } ->
                HH.li
                  [ css $ itemCss true (isSelected id)
                  , HE.onClick \_ -> Select $ currentPath <> [ id ]
                  , HE.onMouseEnter (SetPath currentPath)
                  ]
                  [ highlightSearch label
                  ]

              Parent { label, selectable, id } ->
                let
                  newPath = currentPath <> [ id ]
                in
                  HH.li
                    ( [ css $ itemCss selectable (isSelected id)
                      , HE.onMouseEnter (SetPath newPath)
                      ]
                        <> guard selectable
                          [ HE.onClick \_ -> Select newPath
                          ]
                    )
                    [ HH.div [] [ highlightSearch label ]
                    , chevronRight []
                    ]

    isSelected :: id -> Boolean
    isSelected id = Just id == head pathToFollow

    childrenMay = case uncons pathToFollow of
      Just { head, tail } ->
        items # findMap case _ of
          Parent { id, children } | head == id ->
            Just
              { tail
              , id
              , children
              }
          _ -> Nothing
      _ -> Nothing

    getLabel = case _ of
      Node { label } -> label
      Parent { label } -> label

  handleAction :: (Action id) -> H.HalogenM (State id) (Action id) _ _ m Unit
  handleAction = case _ of
    Init -> pure unit

    Receive input ->
      H.modify_ \st -> st
        { input = input
        }

    ToggleOpen -> H.modify_ \st ->
      st
        { open = not st.open
        , searches = Map.empty :: Map (Array id) String
        }

    SetPath path ev -> do
      liftEffect $ stopPropagation $ MouseEvent.toEvent ev
      H.modify_ _ { path = path }

    Select path -> do
      H.modify_ _
        { open = false
        , searches = Map.empty :: Map (Array id) String
        }
      H.raise path

    SetSearch ids search ->
      H.modify_ \st -> st
        { searches =
            Map.insert ids search st.searches
        }
