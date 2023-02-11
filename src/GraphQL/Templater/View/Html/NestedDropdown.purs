module GraphQL.Templater.View.Html.NestedDropdown
  ( DropdownItem(..)
  , Input
  , nestedDropdown
  ) where

import Prelude

import Data.Array (find, findMap, head, uncons)
import Data.Lazy (Lazy, force)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect.Class (class MonadEffect, liftEffect)
import GraphQL.Templater.View.Html.Icons (chevronRight)
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

type Input id =
  { label :: String
  , items :: Array (DropdownItem id)
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
  }

nestedDropdown
  :: forall id m q
   . MonadEffect m
  => Eq id
  => H.Component q (Input id) (Array id) m
nestedDropdown =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }
  where
  initialState :: Input id -> State id
  initialState =
    { input: _
    , open: false
    , path: []
    }

  render :: State id -> HH.HTML _ (Action id)
  render state@{ input: { items } } =
    HH.div [ css "relative mt-1" ]
      [ HH.button
          [ css "relative w-full cursor-pointer rounded-md border border-gray-300 bg-white py-2 pl-3 pr-10 text-left shadow-sm focus:border-blue-500 focus:outline-none focus:ring-1 focus:ring-blue-500 sm:text-sm"
          , HE.onClick \_ -> ToggleOpen
          ]
          [ HH.text state.input.label 
          ]
      , whenElem state.open \_ ->
          HH.div [css "absolute flex z-10 mt-1 max-h-[32rem] w-full "] $
            renderItems state [] state.path items
      ]

  renderItems :: State id -> Array id -> Array id -> Array (DropdownItem id) -> Array (HH.HTML _ (Action id))
  renderItems state currentPath pathToFollow items =
    [ HH.ul
        [ css "overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm" ]
        $ items <#> \item ->
            let
              itemCss selectable selected =
                "text-gray-900 relative flex w-full justify-between select-none py-2 pl-3 pr-3 hover:bg-blue-100 "
                  <> guard selectable " cursor-pointer"
                  <> guard selected " bg-blue-200"

            in
              case item of
                Node { label, id } ->
                  HH.li
                    [ css $ itemCss true (isSelected id)
                    , HE.onClick \_ -> Select $ currentPath <> [ id ]
                    , HE.onMouseEnter (SetPath currentPath)
                    ]
                    [ HH.text label
                    ]

                Parent { label, selectable, id } ->
                  let
                    newPath = currentPath <> [ id ]
                  in
                    HH.li
                      ( [ css $ itemCss true (isSelected id)
                        ]
                          <> guard selectable
                            [ HE.onClick \_ -> Select newPath
                            , HE.onMouseEnter (SetPath newPath)
                            ]
                      )
                      [ HH.div [] [HH.text label]
                      , chevronRight []
                      ]
    ]
      <> case childrenMay of
        Just { children, tail } ->
          renderItems state pathToFollow tail (force children)
        _ -> []

    where
    isSelected :: id -> Boolean 
    isSelected id = Just id == head pathToFollow

    childrenMay = case uncons pathToFollow of
      Just { head, tail } ->
        items # findMap case _ of
          Parent { id, children } | head == id -> Just { children, tail }
          _ -> Nothing
      _ -> Nothing

  -- <div class="relative mt-1">
  --   <button type="button" class="relative w-full cursor-default rounded-md border border-gray-300 bg-white py-2 pl-3 pr-10 text-left shadow-sm focus:border-blue-500 focus:outline-none focus:ring-1 focus:ring-blue-500 sm:text-sm" aria-haspopup="listbox" aria-expanded="true" aria-labelledby="listbox-label">
  --     <span class="block truncate">Tom Cook</span>
  --     <span class="pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2">
  --       <!-- Heroicon name: mini/chevron-up-down -->
  --       <svg class="h-5 w-5 text-gray-400" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
  --         <path fill-rule="evenodd" d="M10 3a.75.75 0 01.55.24l3.25 3.5a.75.75 0 11-1.1 1.02L10 4.852 7.3 7.76a.75.75 0 01-1.1-1.02l3.25-3.5A.75.75 0 0110 3zm-3.76 9.2a.75.75 0 011.06.04l2.7 2.908 2.7-2.908a.75.75 0 111.1 1.02l-3.25 3.5a.75.75 0 01-1.1 0l-3.25-3.5a.75.75 0 01.04-1.06z" clip-rule="evenodd" />
  --       </svg>
  --     </span>
  --   </button>
  -- <ul class="absolute z-10 mt-1 max-h-60 w-full overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm" tabindex="-1" role="listbox" aria-labelledby="listbox-label" aria-activedescendant="listbox-option-3">
  --   <!--
  --     Select option, manage highlight styles based on mouseenter/mouseleave and keyboard navigation.

  --     Highlighted: "text-white bg-blue-600", Not Highlighted: "text-gray-900"
  --   -->
  --   <li class="text-gray-900 relative cursor-default select-none py-2 pl-3 pr-9" id="listbox-option-0" role="option">
  --     <!-- Selected: "font-semibold", Not Selected: "font-normal" -->
  --     <span class="font-normal block truncate">Wade Cooper</span>

  --     <!--
  --       Checkmark, only display for selected option.

  --       Highlighted: "text-white", Not Highlighted: "text-blue-600"
  --     -->
  --     <span class="text-blue-600 absolute inset-y-0 right-0 flex items-center pr-4">
  --       <!-- Heroicon name: mini/check -->
  --       <svg class="h-5 w-5" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
  --         <path fill-rule="evenodd" d="M16.704 4.153a.75.75 0 01.143 1.052l-8 10.5a.75.75 0 01-1.127.075l-4.5-4.5a.75.75 0 011.06-1.06l3.894 3.893 7.48-9.817a.75.75 0 011.05-.143z" clip-rule="evenodd" />
  --       </svg>
  --     </span>
  --   </li>

  --   <!-- More items... -->
  -- </ul>

  handleAction :: (Action id) -> H.HalogenM (State id) (Action id) _ _ m Unit
  handleAction = case _ of
    Init -> pure unit

    Receive input ->
      pure unit

    ToggleOpen -> H.modify_ \st -> st { open = not st.open }

    SetPath path ev -> do
      liftEffect $ stopPropagation $ MouseEvent.toEvent ev
      H.modify_ _ { path = path }

    Select path -> H.raise path
-- H.modify_ \_state -> input
-- nestedDropdown :: Array ()