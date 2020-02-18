module WebTiled.Util.File exposing (getLevel)

import Dict
import File exposing (File)
import Html
import Html.Events
import Json.Decode as D
import Set
import Task
import Tiled
import Tiled.Level exposing (Level)
import Tiled.Tileset
import WebTiled.Message exposing (Message(..))
import WebTiled.Util.Tiled as TiledUtil


getLevel : List File -> Cmd Message
getLevel files =
    List.map
        (\file ->
            case File.mime file of
                "application/json" ->
                    File.toString file
                        |> Task.map (Tuple.pair (File.name file))

                _ ->
                    File.toUrl file
                        |> Task.map (Tuple.pair (File.name file))
        )
        files
        |> Task.sequence
        |> Task.attempt
            (\r ->
                case r of
                    Ok fs ->
                        List.foldl parseFile ( Dict.empty, Dict.empty, Dict.empty ) fs
                            |> uncurry3 FilesFromDisk

                    Err err ->
                        FileError err
            )


parseFile ( name, file ) ( levels, tilesets, images ) =
    if String.endsWith ".json" name then
        case D.decodeString Tiled.decode file of
            Ok level ->
                ( Dict.insert name level levels, tilesets, images )

            Err _ ->
                case D.decodeString (Tiled.Tileset.decodeFile -1) file of
                    Ok tileset ->
                        ( levels, Dict.insert name tileset tilesets, images )

                    Err _ ->
                        ( levels, tilesets, images )

    else
        ( levels, tilesets, Dict.insert name file images )


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c
