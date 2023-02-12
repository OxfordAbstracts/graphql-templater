module GraphQL.Templater.View.Html.Icons where

import Prelude ((<>))
import Halogen.HTML

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"

check :: forall p r i. Array (IProp r i) -> HTML p i
check attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M16.7045 4.15347C17.034 4.4045 17.0976 4.87509 16.8466 5.20457L8.84657 15.7046C8.71541 15.8767 8.51627 15.9838 8.30033 15.9983C8.08439 16.0129 7.87271 15.9334 7.71967 15.7804L3.21967 11.2804C2.92678 10.9875 2.92678 10.5126 3.21967 10.2197C3.51256 9.92682 3.98744 9.92682 4.28033 10.2197L8.17351 14.1129L15.6534 4.29551C15.9045 3.96603 16.3751 3.90243 16.7045 4.15347Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronDoubleDown :: forall p r i. Array (IProp r i) -> HTML p i
chevronDoubleDown attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M14.7698 4.20938C15.0684 4.49647 15.0777 4.97125 14.7906 5.26983L10.5406 9.76983C10.3992 9.91689 10.204 10 10 10C9.79599 10 9.60078 9.91689 9.45937 9.76983L5.20937 5.26983C4.92228 4.97125 4.93159 4.49647 5.23017 4.20938C5.52875 3.92228 6.00353 3.93159 6.29062 4.23017L10 8.16792L13.7094 4.23017C13.9965 3.93159 14.4713 3.92228 14.7698 4.20938ZM14.7698 10.2094C15.0684 10.4965 15.0777 10.9713 14.7906 11.2698L10.5406 15.7698C10.3992 15.9169 10.204 16 10 16C9.79599 16 9.60078 15.9169 9.45937 15.7698L5.20937 11.2698C4.92228 10.9713 4.93159 10.4965 5.23017 10.2094C5.52875 9.92228 6.00353 9.93159 6.29062 10.2302L10 14.1679L13.7094 10.2302C13.9965 9.93159 14.4713 9.92228 14.7698 10.2094Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronDoubleLeft :: forall p r i. Array (IProp r i) -> HTML p i
chevronDoubleLeft attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M15.7906 14.7698C15.5035 15.0684 15.0287 15.0777 14.7302 14.7906L10.2302 10.5406C10.0831 10.3992 10 10.204 10 10C10 9.79599 10.0831 9.60078 10.2302 9.45938L14.7302 5.20937C15.0287 4.92228 15.5035 4.93159 15.7906 5.23017C16.0777 5.52875 16.0684 6.00353 15.7698 6.29062L11.8321 10L15.7698 13.7094C16.0684 13.9965 16.0777 14.4713 15.7906 14.7698ZM9.79062 14.7698C9.50353 15.0684 9.02875 15.0777 8.73017 14.7906L4.23017 10.5406C4.08311 10.3992 4 10.204 4 10C4 9.79599 4.08311 9.60078 4.23017 9.45938L8.73017 5.20938C9.02875 4.92228 9.50353 4.93159 9.79062 5.23017C10.0777 5.52875 10.0684 6.00353 9.76983 6.29062L5.83208 10L9.76983 13.7094C10.0684 13.9965 10.0777 14.4713 9.79062 14.7698Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronDoubleRight :: forall p r i. Array (IProp r i) -> HTML p i
chevronDoubleRight attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M10.2094 14.7698C9.92228 14.4713 9.93159 13.9965 10.2302 13.7094L14.1679 10L10.2302 6.29062C9.93159 6.00353 9.92228 5.52875 10.2094 5.23017C10.4965 4.93159 10.9713 4.92228 11.2698 5.20937L15.7698 9.45937C15.9169 9.60078 16 9.79599 16 10C16 10.204 15.9169 10.3992 15.7698 10.5406L11.2698 14.7906C10.9713 15.0777 10.4965 15.0684 10.2094 14.7698Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ], elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M4.20938 14.7698C3.92228 14.4713 3.93159 13.9965 4.23017 13.7094L8.16792 10L4.23017 6.29062C3.93159 6.00353 3.92228 5.52875 4.20938 5.23017C4.49647 4.93159 4.97125 4.92228 5.26983 5.20937L9.76983 9.45937C9.91689 9.60078 10 9.79599 10 10C10 10.204 9.91689 10.3992 9.76983 10.5406L5.26983 14.7906C4.97125 15.0777 4.49647 15.0684 4.20938 14.7698Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronDoubleUp :: forall p r i. Array (IProp r i) -> HTML p i
chevronDoubleUp attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M5.23017 15.7906C4.93159 15.5035 4.92228 15.0287 5.20938 14.7302L9.45938 10.2302C9.60078 10.0831 9.79599 10 10 10C10.204 10 10.3992 10.0831 10.5406 10.2302L14.7906 14.7302C15.0777 15.0287 15.0684 15.5035 14.7698 15.7906C14.4713 16.0777 13.9965 16.0684 13.7094 15.7698L10 11.8321L6.29063 15.7698C6.00353 16.0684 5.52875 16.0777 5.23017 15.7906ZM5.23017 9.79062C4.93159 9.50353 4.92228 9.02875 5.20938 8.73017L9.45938 4.23017C9.60078 4.08311 9.79599 4 10 4C10.204 4 10.3992 4.08311 10.5406 4.23017L14.7906 8.73017C15.0777 9.02875 15.0684 9.50353 14.7698 9.79062C14.4713 10.0777 13.9965 10.0684 13.7094 9.76983L10 5.83208L6.29063 9.76983C6.00353 10.0684 5.52875 10.0777 5.23017 9.79062Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronDown :: forall p r i. Array (IProp r i) -> HTML p i
chevronDown attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M5.23017 7.20938C5.52875 6.92228 6.00353 6.93159 6.29063 7.23017L10 11.1679L13.7094 7.23017C13.9965 6.93159 14.4713 6.92228 14.7698 7.20938C15.0684 7.49647 15.0777 7.97125 14.7906 8.26983L10.5406 12.7698C10.3992 12.9169 10.204 13 10 13C9.79599 13 9.60078 12.9169 9.45938 12.7698L5.20938 8.26983C4.92228 7.97125 4.93159 7.49647 5.23017 7.20938Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronLeft :: forall p r i. Array (IProp r i) -> HTML p i
chevronLeft attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M12.7906 5.23017C13.0777 5.52875 13.0684 6.00353 12.7698 6.29063L8.83208 10L12.7698 13.7094C13.0684 13.9965 13.0777 14.4713 12.7906 14.7698C12.5035 15.0684 12.0287 15.0777 11.7302 14.7906L7.23017 10.5406C7.08311 10.3992 7 10.204 7 10C7 9.79599 7.08311 9.60078 7.23017 9.45938L11.7302 5.20938C12.0287 4.92228 12.5035 4.93159 12.7906 5.23017Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronRight :: forall p r i. Array (IProp r i) -> HTML p i
chevronRight attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M7.20938 14.7698C6.92228 14.4713 6.93159 13.9965 7.23017 13.7094L11.1679 10L7.23017 6.29062C6.93159 6.00353 6.92228 5.52875 7.20938 5.23017C7.49647 4.93159 7.97125 4.92228 8.26983 5.20937L12.7698 9.45937C12.9169 9.60078 13 9.79599 13 10C13 10.204 12.9169 10.3992 12.7698 10.5406L8.26983 14.7906C7.97125 15.0777 7.49647 15.0684 7.20938 14.7698Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronUpDown :: forall p r i. Array (IProp r i) -> HTML p i
chevronUpDown attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M10 3C10.2086 3 10.4077 3.08684 10.5496 3.23966L13.7996 6.73966C14.0815 7.0432 14.0639 7.51774 13.7603 7.7996C13.4568 8.08145 12.9823 8.06387 12.7004 7.76034L10 4.85221L7.2996 7.76034C7.01775 8.06387 6.5432 8.08145 6.23966 7.79959C5.93613 7.51774 5.91856 7.04319 6.20041 6.73966L9.45041 3.23966C9.59232 3.08684 9.79145 3 10 3ZM6.23967 12.2004C6.5432 11.9186 7.01775 11.9361 7.2996 12.2397L10 15.1478L12.7004 12.2397C12.9823 11.9361 13.4568 11.9186 13.7603 12.2004C14.0639 12.4823 14.0815 12.9568 13.7996 13.2603L10.5496 16.7603C10.4077 16.9132 10.2086 17 10 17C9.79145 17 9.59232 16.9132 9.45041 16.7603L6.20041 13.2603C5.91856 12.9568 5.93613 12.4823 6.23967 12.2004Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]

chevronUp :: forall p r i. Array (IProp r i) -> HTML p i
chevronUp attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "width") "20"
  , attr (AttrName "height") "20"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "none"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "clip-rule") "evenodd"
    , attr (AttrName "d") "M14.7698 12.7906C14.4713 13.0777 13.9965 13.0684 13.7094 12.7698L10 8.83208L6.29062 12.7698C6.00353 13.0684 5.52875 13.0777 5.23017 12.7906C4.93159 12.5035 4.92228 12.0287 5.20937 11.7302L9.45937 7.23017C9.60078 7.08311 9.79599 7 10 7C10.204 7 10.3992 7.08311 10.5406 7.23017L14.7906 11.7302C15.0777 12.0287 15.0684 12.5035 14.7698 12.7906Z"
    , attr (AttrName "fill") "#0F172A"
    ]
    [ 
    ]
  ]
