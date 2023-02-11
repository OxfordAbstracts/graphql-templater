module GraphQL.Templater.View.Html.Utils where


import Prelude

import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties (class_)




css
  :: forall r a
   . String
  -> IProp
       ( class :: String
       | r
       )
       a
css = class_ <<< ClassName


whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""