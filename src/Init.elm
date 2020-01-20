module Init exposing (Editor, Level(..), Model, initEditor)

import Dict exposing (Dict)
import IDE.UI.Hotkey as Hotkey exposing (Hotkey)
import IDE.UI.Tree as UI exposing (Tree)
import Tiled.Level
import WebTiled.DropFiles exposing (DropInfo, File)
import WebTiled.Kind exposing (Kind)
import WebTiled.Panel as Panel
import WebTiled.Panel2 as Panel2


type alias Editor msg =
    { ui : UI.Tree Kind
    , modal : Maybe (UI.Tree Kind)
    , hotkey : Hotkey.Model msg
    , relUrl : String
    , editor : Panel.Model
    , files : Dict String File
    , inStore : Dict String (Maybe String)
    }


type Level
    = Loading
    | Succeed Tiled.Level.Level
    | Fail String


type alias Model msg =
    { level : Level
    , editor : Editor msg
    , size : { w : Int, h : Int }

    -------
    , state : Panel2.Model
    , ui2 : Tree Kind
    }


initEditor : String -> Editor msg
initEditor url =
    let
        relUrl =
            String.split "/" url
                |> List.reverse
                |> List.drop 1
                |> (::) ""
                |> List.reverse
                |> String.join "/"

        topMenu =
            Panel.block.topMenu

        topToolbar2 =
            UI.fromList ( Panel.block.mainTools, [ Panel.block.layerTools ] )

        leftSide2 =
            UI.fromList ( Panel.block.levelProperties, [ Panel.block.properties ] )

        center2 =
            UI.fromList ( Panel.block.render, [] )

        rightSide2 =
            UI.fromList ( Panel.block.layers, [ Panel.block.tilesets ] )

        mainStuff =
            UI.fromList ( leftSide2, [ center2, rightSide2, Panel.block.fileManager ] )

        allTogether =
            UI.fromList ( topMenu, [ topToolbar2, mainStuff ] )
    in
    { ui = allTogether
    , modal = Just Panel2.preferences
    , hotkey = initMacHotkey
    , relUrl = relUrl
    , editor = Panel.init
    , files = Dict.empty
    , inStore = Dict.empty
    }


initMacHotkey =
    Dict.empty



--Dict.fromList [ ( "⌘S", Action "Save" ) ]
