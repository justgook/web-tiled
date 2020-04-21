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
import WebGL.Texture as Texture
import WebTiled.Message exposing (Message(..))
import WebTiled.Render exposing (textureOption)
import WebTiled.Util.Tiled as TiledUtil


type FileMime
    = Json String
    | Bytes String


getLevel : List File -> Cmd Message
getLevel =
    List.map
        (\file ->
            case File.mime file of
                "application/json" ->
                    File.toString file
                        |> Task.map (Json >> Tuple.pair (File.name file))

                _ ->
                    File.toUrl file
                        |> Task.map (Bytes >> Tuple.pair (File.name file))
        )
        >> Task.sequence
        >> Task.attempt
            (\r ->
                case r of
                    Ok files ->
                        case List.foldl parseFile ( Nothing, Dict.empty, Dict.empty ) files of
                            ( Just level, tilesets, images ) ->
                                FilesFromDisk level tilesets images

                            ( Nothing, _, _ ) ->
                                FileError "Missing Level file"

                    Err err ->
                        FileError err
            )


parseFile :
    ( String, FileMime )
    -> ( Maybe Level, Dict.Dict String Tiled.Tileset.Tileset, Dict.Dict String String )
    -> ( Maybe Level, Dict.Dict String Tiled.Tileset.Tileset, Dict.Dict String String )
parseFile ( name, file_ ) ( mLevel, tilesets, images ) =
    case file_ of
        Json file ->
            case D.decodeString Tiled.decode file of
                Ok level ->
                    ( Just level, tilesets, images )

                Err _ ->
                    case D.decodeString (Tiled.Tileset.decodeFile -1) file of
                        Ok tileset ->
                            ( mLevel, Dict.insert name tileset tilesets, images )

                        Err _ ->
                            ( mLevel, tilesets, images )

        Bytes file ->
            ( mLevel, tilesets, Dict.insert name file images )
