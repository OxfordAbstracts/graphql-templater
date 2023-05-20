module GraphQL.Templater.View.Component.SelectMenu
  ( Input
  , Output(..)
  , selectMenu
  , selectMenuComponent
  , selectNative
  ) where

import Prelude

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy)
import GraphQL.Templater.View.Html.Utils (css, whenElem)
import Halogen as H
import Halogen.HTML (AttrName(..), ElemName(..), Namespace(..), attr, elementNS)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record

data Action
  = Init
  | Receive Input
  | ToggleOpen
  | Select String
  | SelectNone

data Output = ValueSelected String | NoneSelected

type Input =
  { label :: String
  , noneSelectedLabel :: String
  , selectedValue :: Maybe String
  , options :: Array String
  }

type State =
  { label :: String
  , noneSelectedLabel :: String
  , selectedValue :: Maybe String
  , options :: Array String
  , open :: Boolean
  }

selectMenuComponent :: forall m q. H.Component q Input Output m
selectMenuComponent =
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
  initialState = Record.merge
    { open: false
    }

  render :: State -> HH.HTML _ Action
  render state =
    selectMenu $ Record.merge state
      { onChange: Select
      , toggleOpen: ToggleOpen
      , selectNone: Just SelectNone
      }

  handleAction :: Action -> H.HalogenM State Action _ _ m Unit
  handleAction = case _ of
    Init -> pure unit

    ToggleOpen -> do
      H.modify_ \st -> st { open = not st.open }

    Select value -> do
      H.modify_ _ { selectedValue = Just value, open = false }
      H.raise $ ValueSelected value

    SelectNone -> do
      H.modify_ _ { selectedValue = Nothing, open = false }
      H.raise NoneSelected

    Receive input ->
      H.modify_ \st -> Record.merge
        { open: st.open
        }
        input

selectMenu
  :: forall w i
   . { label :: String
     , noneSelectedLabel :: String
     , toggleOpen :: i
     , selectNone :: Maybe i
     , onChange :: String -> i
     , selectedValue :: Maybe String
     , options :: Array String
     , open :: Boolean
     }
  -> HH.HTML w i
selectMenu
  { label
  , onChange
  , noneSelectedLabel
  , toggleOpen
  , selectNone
  , selectedValue
  , options
  , open
  } =
  HH.div []
    [ HH.label
        [ css "block text-sm font-medium leading-6 text-gray-900" ]
        [ HH.text label ]
    , HH.div [ css "relative mt-2" ]
        [ HH.button
            [ css "relative w-full cursor-pointer rounded-md bg-white py-1.5 pl-3 pr-10 text-left text-gray-900 shadow-sm ring-1 ring-inset ring-gray-300 focus:outline-none focus:ring-2 focus:ring-blue-600 sm:text-sm sm:leading-6"
            , HE.onClick \_ -> toggleOpen
            ]
            [ HH.span [ css "block truncate" ] [ HH.text $ fromMaybe noneSelectedLabel selectedValue ]
            , HH.span [ css "pointer-events-none absolute inset-y-0 right-0 flex items-center pr-2" ]
                [ selectIcon
                ]
            ]

        ]
    , whenElem open \_ ->
        HH.ul
          [ css "absolute z-10 mt-1 max-h-60 w-fit overflow-auto rounded-md bg-white py-1 text-base shadow-lg ring-1 ring-black ring-opacity-5 focus:outline-none sm:text-sm"
          ]
          $
            selectNoneHtml <>
              optionsHtml
    ]
  where
  selectNoneHtml = case selectNone of
    Nothing -> mempty
    Just i ->
      [ HH.li
          [ css "h-8 text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-100"
          , HE.onClick (\_ -> i)
          ]
          [ HH.span
              [ css "block truncate font-semibold" ]
              [ HH.text "" ]
          ]
      ]
  optionsHtml = options <#> \value ->
    let
      selected = Just value == selectedValue
    in
      HH.li
        [ css "text-gray-900 relative cursor-pointer select-none py-2 pl-3 pr-9 hover:bg-gray-100"
        , HE.onClick (\_ -> onChange value)
        ]
        [ HH.span
            [ css $ "block truncate " <> if selected then "font-semibold" else "font-normal" ]
            [ HH.text value ]
        ]

selectNative
  :: forall w i
   . String
  -> (String -> i)
  -> Maybe String
  -> Array String
  -> HH.HTML w i
selectNative label onChange selectedValue options =
  HH.div
    []
    [ HH.label
        [ css "block text-sm font-medium leading-6 text-gray-900" ]
        [ HH.text label ]
    , HH.text $ show selectedValue
    , HH.select
        ( [ css "mt-2 block w-full rounded-md border-0 py-1.5 pl-3 pr-10 text-gray-900 ring-1 ring-inset ring-gray-300 focus:ring-2 focus:ring-blue-600 sm:text-sm sm:leading-6"
          , HE.onValueChange (onChange <<< spy "select")
          ]
            <> (selectedValue # foldMap (pure <<< HP.value))
        )
        $
          options <#> \value ->
            let
              selected = Just value == selectedValue
            in
              HH.option
                [ HP.value value
                , HP.selected selected
                ]
                [ HH.text value
                , HH.text $ show selected
                ]
    ]

selectIcon :: forall w69 i70. HH.HTML w69 i70
selectIcon = elementNS ns (ElemName "svg")
  ( [ attr (AttrName "class") "h-5 w-5 text-gray-400"
    , attr (AttrName "viewBox") "0 0 20 20"
    , attr (AttrName "fill") "currentColor"
    , attr (AttrName "aria-hidden") "true"
    ]
  )
  [ elementNS ns (ElemName "path")
      [ attr (AttrName "fill-rule") "evenodd"
      , attr (AttrName "d") "M10 3a.75.75 0 01.55.24l3.25 3.5a.75.75 0 11-1.1 1.02L10 4.852 7.3 7.76a.75.75 0 01-1.1-1.02l3.25-3.5A.75.75 0 0110 3zm-3.76 9.2a.75.75 0 011.06.04l2.7 2.908 2.7-2.908a.75.75 0 111.1 1.02l-3.25 3.5a.75.75 0 01-1.1 0l-3.25-3.5a.75.75 0 01.04-1.06z"
      , attr (AttrName "clip-rule") "evenodd"
      ]
      [
      ]
  ]

checkMark :: forall p r i. HH.HTML p i
checkMark =
  elementNS ns (ElemName "svg")
    ( [ attr (AttrName "class") "h-5 w-5"
      , attr (AttrName "viewBox") "0 0 20 20"
      , attr (AttrName "fill") "currentColor"
      , attr (AttrName "aria-hidden") "true"
      ]
    )
    [ elementNS ns (ElemName "path")
        [ attr (AttrName "fill-rule") "evenodd"
        , attr (AttrName "d") "M16.704 4.153a.75.75 0 01.143 1.052l-8 10.5a.75.75 0 01-1.127.075l-4.5-4.5a.75.75 0 011.06-1.06l3.894 3.893 7.48-9.817a.75.75 0 011.05-.143z"
        , attr (AttrName "clip-rule") "evenodd"
        ]
        [
        ]
    ]

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"
