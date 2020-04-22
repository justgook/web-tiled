module WebTiled.RenameMe exposing (shortcuts)

import Dict exposing (Dict)
import WebTiled.Message exposing (Message(..), PreferencesTab(..))


shortcuts : Dict String { msg : Message, shortcut : String }
shortcuts =
    Dict.fromList
        [ ( "Close Modal", { msg = CloseModal, shortcut = "" } )
        , ( "New Map", { msg = NewMap, shortcut = "⌥⌘N" } )
        , ( "New Tileset", { msg = NewTileset, shortcut = "" } )
        , ( "New World", { msg = NewWorld, shortcut = "" } )
        , ( "Open", { msg = Open, shortcut = "⌘O" } )
        , ( "Open Url", { msg = OpenUrl, shortcut = "" } )
        , ( "Save", { msg = Save, shortcut = "⌘S" } )
        , ( "Save As", { msg = SaveAs, shortcut = "⇧⌘S" } )
        , ( "Export", { msg = Export, shortcut = "⌘E" } )
        , ( "Export As", { msg = ExportAs, shortcut = "" } )
        , ( "Export As Image", { msg = ExportAsImage, shortcut = "" } )
        , ( "Reload", { msg = Reload, shortcut = "" } )
        , ( "Preferences", { msg = ShowPreferences Keyboard, shortcut = "⌘," } )
        , ( "Show Properties", { msg = ToggleProperties, shortcut = "" } )
        , ( "Show Layers", { msg = ToggleLayers, shortcut = "" } )
        , ( "Show Tilesets", { msg = ToggleTilesets, shortcut = "" } )
        , ( "Show FileManager", { msg = ShowFileManager, shortcut = "" } )
        , ( "Show Toolbar", { msg = ToggleToolbar, shortcut = "" } )
        , ( "Show StatusBar", { msg = ToggleStatusBar, shortcut = "" } )
        , ( "Full Screen", { msg = FullScreen, shortcut = "" } )
        , ( "Clear View", { msg = ClearView, shortcut = "" } )
        , ( "Object Types Editor", { msg = ObjectTypesEditor, shortcut = "" } )
        , ( "Show Grid", { msg = ShowGrid, shortcut = "⌘G" } )
        , ( "Show Tile Animations", { msg = ShowTileAnimations, shortcut = "" } )
        , ( "Show Tile Collision Shapes", { msg = ShowTileCollisionShapes, shortcut = "" } )
        , ( "Resize Map", { msg = ResizeMap, shortcut = "" } )
        , ( "AutoMap", { msg = AutoMap, shortcut = "" } )
        , ( "AutoMap Editor", { msg = AutoMapEditor, shortcut = "" } )
        , ( "Map Properties", { msg = ShowMapProperties, shortcut = "" } )
        , ( "New Tile Layer", { msg = NewTileLayer, shortcut = "" } )
        , ( "New Object Layer", { msg = NewObjectLayer, shortcut = "" } )
        , ( "New Image Layer", { msg = NewImageLayer, shortcut = "" } )
        , ( "Duplicate Layer", { msg = DuplicateLayer, shortcut = "" } )
        , ( "Remove Layer", { msg = RemoveLayer, shortcut = "" } )
        , ( "Select Previous Layer", { msg = SelectPreviousLayer, shortcut = "" } )
        , ( "Select Next Layer", { msg = SelectNextLayer, shortcut = "" } )
        , ( "Raise Layer", { msg = RaiseLayer, shortcut = "" } )
        , ( "Lower Layer", { msg = LowerLayer, shortcut = "" } )
        , ( "Layer Properties", { msg = ShowLayerProperties, shortcut = "" } )
        ]