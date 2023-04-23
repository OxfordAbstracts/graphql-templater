module GraphQL.Templater.View.Component.SelectMenu where

import Prelude

import GraphQL.Templater.View.Html.Utils (css)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

selectMenu
  :: forall w i
   . String
  -> (String -> i)
  -> Array
       { selected :: Boolean
       , value :: String
       }
  -> HH.HTML w i
selectMenu label onChange options =
  HH.div
    []
    [ HH.label
        [ css "block text-sm font-medium leading-6 text-gray-900" ]
        [ HH.text label ]
    , HH.select
        [ css "mt-2 block w-full rounded-md border-0 py-1.5 pl-3 pr-10 text-gray-900 ring-1 ring-inset ring-gray-300 focus:ring-2 focus:ring-blue-600 sm:text-sm sm:leading-6"
        , HE.onValueChange onChange
        ]
        $
          options <#> \{ value, selected } ->
            HH.option
              [ HP.value value
              , HP.selected selected
              ]
              [ HH.text value ]
    ]
-- <div>
--   <label for="location" class="block text-sm font-medium leading-6 text-gray-900">Location</label>
--   <select id="location" name="location" class="mt-2 block w-full rounded-md border-0 py-1.5 pl-3 pr-10 text-gray-900 ring-1 ring-inset ring-gray-300 focus:ring-2 focus:ring-blue-600 sm:text-sm sm:leading-6">
--     <option>United States</option>
--     <option selected>Canada</option>
--     <option>Mexico</option>
--   </select>
-- </div>