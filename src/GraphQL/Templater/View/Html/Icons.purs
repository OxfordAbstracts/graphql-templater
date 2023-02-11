module GraphQL.Templater.View.Html.Icons where

import Prelude ((<>))
import Halogen.HTML

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"

chevronRight :: forall p r i. Array (IProp r i) -> HTML p i
chevronRight attrs =
  elementNS ns (ElemName "svg")
    ( attrs <>
        [ attr (AttrName "width") "20"
        , attr (AttrName "height") "20"
        , attr (AttrName "viewBox") "0 0 20 20"
        , attr (AttrName "fill") "none"
        ]
    )
    [ elementNS ns (ElemName "path")
        [ attr (AttrName "fill-rule") "evenodd"
        , attr (AttrName "clip-rule") "evenodd"
        , attr (AttrName "d") "M7.20938 14.7698C6.92228 14.4713 6.93159 13.9965 7.23017 13.7094L11.1679 10L7.23017 6.29062C6.93159 6.00353 6.92228 5.52875 7.20938 5.23017C7.49647 4.93159 7.97125 4.92228 8.26983 5.20937L12.7698 9.45937C12.9169 9.60078 13 9.79599 13 10C13 10.204 12.9169 10.3992 12.7698 10.5406L8.26983 14.7906C7.97125 15.0777 7.49647 15.0684 7.20938 14.7698Z"
        , attr (AttrName "fill") "#0F172A"
        ]
        [
        ]
    ]
