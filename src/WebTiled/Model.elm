module WebTiled.Model exposing
    ( CurrentLevel(..)
    , Kind(..)
    , LevelFrom(..)
    , Model
    , PropertiesFor(..)
    , init
    )

import Dict exposing (Dict)
import IDE.UI.Layout as UI
import Json.Decode as D
import Tiled.Level
import Tiled.Tileset
import WebTiled.Message exposing (PreferencesTab(..))


type alias Model =
    { selectedLayers : List Int
    , selectedTileset : Int
    , settings : PreferencesTab
    , propertiesFocus : PropertiesFor
    , level : CurrentLevel
    , size : { w : Int, h : Int }
    , modal : Maybe (UI.Layout Kind)
    , layout : UI.Layout Kind

    --, files : Dict String ParsedFile
    --, inStore : Dict String (Maybe String)
    ----
    , remoteStorage : RemoteStorage
    , version : String
    }


init : D.Value -> Model
init flags =
    let
        version =
            D.decodeValue (D.field "version" D.string) flags
                |> Result.withDefault "1.0.0"
    in
    { selectedLayers = [ 0 ]
    , selectedTileset = 0
    , settings = Account
    , propertiesFocus = LevelProps
    , level = LevelNone
    , size = { w = 800, h = 600 }
    , modal = Nothing --Just (UI.node FakeProgress |> UI.setLimits { yMax = Just 200, yMin = 200, xMax = Just 200, xMin = 200 })
    , layout = UI.node Error
    , remoteStorage = Offline
    , version = version
    }


type LevelFrom
    = UrlLevel String
    | DiskLevel (Dict String String)
    | RemoteStorageLevel (Dict String String)


type CurrentLevel
    = LevelComplete LevelFrom Tiled.Level.Level (Dict Int Tiled.Tileset.Tileset)
    | LevelPartial LevelFrom Tiled.Level.Level (Dict Int Tiled.Tileset.Tileset) (List String)
    | LevelLoading LevelFrom Tiled.Level.Level (Dict Int (Maybe Tiled.Tileset.Tileset)) (Dict String (Maybe String))
    | LevelNone


type alias Images =
    Dict String String


type PropertiesFor
    = TilesetProps Int
    | TileProps Int
    | LayerProps Int
    | ObjectProps Int
    | LevelProps


type Kind
    = MainTools
    | TileLayerTools
    | CloudTools
    | Statusbar
    | ObjectTools
    | Properties
    | Layers
    | Tilesets
    | Preview
    | FileManager
    | TopMenu
    | Preferences
    | FakeProgress
    | Error


type RemoteStorage
    = Offline
    | Connecting
    | Online
