module GraphQL.Templater.View.Html.Input where

import GraphQL.Templater.View.Html.Utils (css)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

input
  :: forall w i
   . { label :: String
     , value :: String
     , placeholder :: String
     , onInput :: String -> i
     }
  -> HH.HTML w i
input { label, value, placeholder, onInput } = HH.div
  [ css "relative rounded-md border border-gray-300 px-3 py-2 shadow-sm focus-within:border-blue-600 focus-within:ring-1 focus-within:ring-blue-600" ]
  [ HH.label
      [ css "absolute -top-2 left-2 -mt-px inline-block bg-white px-1 text-xs font-medium text-gray-900"
      , HP.for "test"
      ]
      [ HH.text label
      ]
  , HH.input
      [ css "block w-full border-0 p-0 text-gray-900 placeholder-gray-500 outline-0 focus:ring-0 sm:text-sm"
      , HP.placeholder placeholder
      , HP.value value
      , HE.onValueInput onInput
      , HP.id "test"
      ]
  ]
