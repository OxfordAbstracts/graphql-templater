module GraphQL.Templater.View.Html.Input
  ( input
  , intInput
  , numberInput
  ) where

import Prelude

import DOM.HTML.Indexed as I
import Data.Int as Int
import Data.Maybe (Maybe, maybe)
import Data.Number as Number
import GraphQL.Templater.View.Html.Utils (css)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Record as Record

input
  :: forall w i
   . { label :: String
     , value :: String
     , placeholder :: String
     , onInput :: String -> i
     }
  -> HH.HTML w i
input = input_ <<< Record.merge { props: [] }

numberInput :: forall w i. { label :: String, value :: Maybe Number, onInput :: String -> Maybe Number -> i } -> HH.HTML w i
numberInput { label, value, onInput } = input_
  { label
  , value: maybe "" show value
  , placeholder: ""
  , onInput: \str -> onInput str (Number.fromString str)
  , props: [ HP.type_ InputNumber ]
  }

intInput :: forall w i. { label :: String, value :: Maybe Int, onInput :: String -> Maybe Int -> i } -> HH.HTML w i
intInput { label, value, onInput } = input_
  { label
  , value: maybe "" show value
  , placeholder: ""
  , onInput: \str -> onInput str (Int.fromString str)
  , props:
      [ HP.type_ InputNumber
      , HP.step $ HP.Step 1.0
      ]
  }

input_
  :: forall w i
   . { label :: String
     , value :: String
     , placeholder :: String
     , onInput :: String -> i
     , props :: Array (IProp I.HTMLinput i)
     }
  -> HH.HTML w i
input_ { label, value, placeholder, onInput, props } = HH.div
  [ css "relative rounded-md border border-gray-300 px-3 py-2 shadow-sm focus-within:border-blue-600 focus-within:ring-1 focus-within:ring-blue-600" ]
  [ HH.label
      [ css "absolute -top-2 left-2 -mt-px inline-block bg-white px-1 text-xs font-medium text-gray-900"
      ]
      [ HH.text label
      ]
  , HH.input $
      [ css "block w-full border-0 p-0 text-gray-900 placeholder-gray-500 outline-0 focus:ring-0 sm:text-sm"
      , HP.placeholder placeholder
      , HP.value value
      , HE.onValueInput onInput
      ]
        <> props
  ]
