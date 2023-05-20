module Icons where

import Prelude ((<>))
import Halogen.HTML

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"


iconX :: forall p r i. Array (IProp r i) -> HTML p i
iconX attrs =
  elementNS ns (ElemName "svg")
  ( attrs <> [ attr (AttrName "class") "h-5 w-5"
  , attr (AttrName "viewBox") "0 0 20 20"
  , attr (AttrName "fill") "currentColor"
  , attr (AttrName "aria-hidden") "true"
  ])
  [ elementNS ns (ElemName "path")
    [ attr (AttrName "fill-rule") "evenodd"
    , attr (AttrName "d") "M16.704 4.153a.75.75 0 01.143 1.052l-8 10.5a.75.75 0 01-1.127.075l-4.5-4.5a.75.75 0 011.06-1.06l3.894 3.893 7.48-9.817a.75.75 0 011.05-.143z"
    , attr (AttrName "clip-rule") "evenodd"
    ]
    [ 
    ]
  ]

