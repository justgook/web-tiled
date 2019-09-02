module WebTiled.Theme exposing (colors)

import Element


toolbar : Element.Color
toolbar =
    Element.rgb255 51 51 51


panel : Element.Color
panel =
    Element.rgb255 38 38 38


colors =
    { background = Element.rgb255 30 30 30
    , font = Element.rgb255 171 178 191
    , panel =
        { background = panel
        , title = toolbar
        }
    , toolbar = { background = toolbar }
    }
