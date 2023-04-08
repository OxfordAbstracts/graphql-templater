module GraphQL.Templater.View.Component.Input where

import GraphQL.Templater.View.Html.Utils (css)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

input :: forall w i. (String -> i) -> String -> HH.HTML w i
input onInput value = HH.input
  [ css "shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
  , HE.onValueInput onInput
  , HP.value value
  ]