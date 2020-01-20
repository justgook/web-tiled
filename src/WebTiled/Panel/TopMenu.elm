module WebTiled.Panel.TopMenu exposing (Model, init, view)

import Generic.TopMenu exposing (MenuItem(..))


type alias Model =
    ()


init =
    ()


view model =
    [ MenuItem "File"
        [ MenuItem "New"
            [ MenuItem "New Map..." []
            , MenuItem "New Tileset..." []
            , MenuItem "New World..." []
            ]
        , MenuItem "Open..." []
        , MenuItem "Open Url.." []
        , Separator
        , MenuItem "Save" []
        , MenuItem "Save As.." []
        , MenuItem "Export" []
        , MenuItem "Export As.." []
        , MenuItem "Export As Image..." []
        , MenuItem "Reload" []
        ]
    , MenuItem "Edit"
        [ MenuItem "Preferences" []
        ]
    , MenuItem "View"
        [ MenuItem "Tool Window"
            [ Checkbox "Properties" True
            , Checkbox "Layers" True
            , Checkbox "Tilesets" True
            , Checkbox "File manager" True
            ]
        , MenuItem "Appearance"
            [ Checkbox "Toolbar" True
            , Checkbox "Status Bar" False
            , Separator
            , Checkbox "Full Screen" False
            , Checkbox "Clear View" False
            ]
        , Separator
        , MenuItem "Object Types Editor" []
        , Separator
        , Checkbox "Show Grid" True
        , Checkbox "Show Tile Animations" True
        , Checkbox "Show Tile Collision Shapes" True
        ]
    , MenuItem "Map"
        [ MenuItem "Resize Map..." []
        , MenuItem "AutoMap" []
        , MenuItem "Map Properties..." []
        ]
    , MenuItem "Layer"
        [ MenuItem "New"
            [ MenuItem "Tile Layer" []
            , MenuItem "Object Layer" []
            , MenuItem "Image Layer" []
            ]
        , MenuItem "Duplicate Layer(s)" []
        , MenuItem "Remove Layer(s)" []
        , Separator
        , MenuItem "Select Previous Layer" []
        , MenuItem "Select Next Layer" []
        , MenuItem "Raise Layer(s)" []
        , MenuItem "Lower Layer(s)" []
        , Separator
        , MenuItem "Layer Properties..." []
        ]
    ]
        |> Generic.TopMenu.view
