module WebTiled.DropFiles exposing (DropInfo, File(..), onDrop)

import Base64
import File
import Html
import Html.Events
import Json.Decode as D
import Task
import Tiled.Level
import Tiled.Tileset


onDrop : Html.Attribute (Task.Task String (List DropInfo))
onDrop =
    Html.Events.custom "drop"
        (D.map
            (\files_ ->
                { message = files_
                , stopPropagation = True
                , preventDefault = True
                }
            )
            files
        )


type File
    = Tileset Tiled.Tileset.Tileset
    | Level Tiled.Level.Level
    | Image String


type alias DropInfo =
    ( String, File, String )


files : D.Decoder (Task.Task String (List DropInfo))
files =
    D.field "dataTransfer" (D.field "files" (D.list File.decoder))
        |> D.map
            (List.foldl
                (\file ->
                    case File.mime file of
                        "image/png" ->
                            (::) <| imgUrl file "image/base64"

                        "application/json" ->
                            (::) <| levelOrTileset file

                        _ ->
                            identity
                )
                []
                >> Task.sequence
            )


levelOrTileset : File.File -> Task.Task String DropInfo
levelOrTileset file =
    File.toString file
        |> Task.andThen
            (\jsonString ->
                case
                    D.decodeString
                        (D.oneOf
                            [ Tiled.Level.decode |> D.map Level
                            , Tiled.Tileset.decodeFile -1000 |> D.map Tileset
                            ]
                        )
                        jsonString
                        |> Result.mapError D.errorToString
                of
                    Ok i ->
                        Task.succeed ( File.name file, i, jsonString )

                    Err i ->
                        Task.fail i
            )


imgUrl : File.File -> String -> Task.Task String DropInfo
imgUrl file mime =
    File.toBytes file
        |> Task.andThen
            (Base64.fromBytes
                >> Maybe.map
                    (\img ->
                        ( File.name file
                        , img
                            |> (++) ("data:" ++ mime ++ ";base64,")
                            |> Image
                        , ""
                        )
                            |> Task.succeed
                    )
                >> Maybe.withDefault (Task.fail "Base64.fromBytes")
            )
