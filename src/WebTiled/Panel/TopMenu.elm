module WebTiled.Panel.TopMenu exposing (view)

import IDE.UI.TopMenu exposing (MenuItem(..))
import WebTiled.Message exposing (Message(..), PreferencesTab(..))


view =
    [ MenuItem True
        Nothing
        "File"
        [ MenuItem False
            Nothing
            "New"
            [ MenuItem False Nothing "New Map..." []
            , MenuItem False Nothing "New Tileset..." []
            , MenuItem False Nothing "New World..." []
            ]
        , MenuItem True (Just Open) "Open..." []
        , MenuItem False Nothing "Open Url..." []
        , Separator
        , MenuItem False Nothing "Save" []
        , MenuItem False Nothing "Save As..." []
        , MenuItem False Nothing "Export" []
        , MenuItem False Nothing "Export As..." []
        , MenuItem False Nothing "Export As Image..." []
        , MenuItem False Nothing "Reload" []
        ]
    , MenuItem True
        Nothing
        "Edit"
        [ MenuItem True (Just (ShowPreferences Account)) "Preferences" []
        ]
    , MenuItem True
        Nothing
        "View"
        [ MenuItem False
            Nothing
            "Tool Window"
            [ Checkbox False "Properties" False
            , Checkbox False "Layers" False
            , Checkbox False "Tilesets" False
            , Checkbox False "File manager" False
            ]
        , MenuItem False
            Nothing
            "Appearance"
            [ Checkbox False "Toolbar" False
            , Checkbox False "Status Bar" True
            , Separator
            , Checkbox False "Full Screen" True
            , Checkbox False "Clear View" True
            ]
        , Separator
        , MenuItem False Nothing "Object Types Editor" []
        , Separator
        , Checkbox False "Show Grid" False
        , Checkbox False "Show Tile Animations" False
        , Checkbox False "Show Tile Collision Shapes" False
        ]
    , MenuItem True
        Nothing
        "Map"
        [ MenuItem False Nothing "Resize Map..." []
        , MenuItem False Nothing "AutoMap" []
        , MenuItem True (Just ShowMapProperties) "Map Properties..." []
        ]
    , MenuItem True
        Nothing
        "Layer"
        [ MenuItem False
            Nothing
            "New"
            [ MenuItem False Nothing "Tile Layer" []
            , MenuItem False Nothing "Object Layer" []
            , MenuItem False Nothing "Image Layer" []
            ]
        , MenuItem False Nothing "Duplicate Layer(s)" []
        , MenuItem False Nothing "Remove Layer(s)" []
        , Separator
        , MenuItem False
            Nothing
            "Scripts"
            [ MenuItem False Nothing "Look Up Texture" []
            , MenuItem False Nothing "Collision Map" []
            ]
        , Separator
        , MenuItem False Nothing "Select Previous Layer" []
        , MenuItem False Nothing "Select Next Layer" []
        , MenuItem False Nothing "Raise Layer(s)" []
        , MenuItem False Nothing "Lower Layer(s)" []
        , Separator
        , MenuItem True (Just ShowLayerProperties) "Layer Properties..." []
        ]
    , MenuItem True
        Nothing
        "Tileset"
        [ MenuItem True (Just ShowTilesetProperties) "Tileset Properties..." [] ]
    , MenuItem True
        Nothing
        "Help"
        [ MenuItem False Nothing "Examples" []
        , MenuItem False Nothing "Tutorial" []
        ]
    ]
        |> IDE.UI.TopMenu.view
