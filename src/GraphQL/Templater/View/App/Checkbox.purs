module GraphQL.Templater.View.Component.Checkbox where


import GraphQL.Templater.View.Html.Utils (css)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP

checkbox :: forall w i. i -> Boolean -> HH.HTML w i
checkbox onClick checked =
  HH.div [ css "flex h-6 items-center" ]
    [ HH.input
        [ HP.type_ InputCheckbox
        , css "h-4 w-4 rounded border-gray-300 text-blue-600 focus:ring-blue-600 cursor-pointer"
        , HP.checked checked
        , HE.onClick \_ -> onClick
        ]
    ]
