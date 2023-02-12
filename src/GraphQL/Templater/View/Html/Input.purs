module GraphQL.Templater.View.Html.Input where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

input
  :: forall w i
   . { label :: String
     , value :: String
     , placeholder :: String
     , onChange :: String -> i
     }
  -> HH.HTML w i
input { label, value, placeholder, onChange } = HH.div
  [ HP.classes $ map HH.ClassName [ "relative", "rounded-md", "border", "border-gray-300", "px-3", "py-2", "shadow-sm", "focus-within:border-blue-600", "focus-within:ring-blue-600" ] ]
  [ HH.label
      [ HP.classes $ map HH.ClassName [ "absolute", "-top-2", "left-2", "-mt-px", "inline-block", "bg-white", "px-1", "text-xs", "font-medium", "text-gray-900" ]
      ]
      [ HH.text label
      ]
  , HH.input
      [ HP.classes $ map HH.ClassName [ "block", "w-full", "border-0", "p-0", "text-gray-900", "placeholder-gray-500", "sm:text-sm" ]
      , HP.placeholder placeholder
      , HP.value value
      , HE.onValueChange onChange
      ]
  ]
