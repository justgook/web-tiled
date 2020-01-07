module WebTiled.DropFiles exposing (File(..), onDrop)

import Base64
import File
import Html
import Html.Events
import Json.Decode as D
import Task
import Tiled.Level
import Tiled.Tileset


onDrop : Html.Attribute (Task.Task String (List ( String, File )))
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


files =
    D.field "dataTransfer" (D.field "files" (D.list File.decoder))
        |> D.map
            (List.foldl
                (\file ->
                    case File.mime file of
                        "image/png" ->
                            (::) <| imgUrl file "image/png"

                        "application/json" ->
                            (::) <| levelOrTileset file

                        _ ->
                            identity
                )
                []
                >> Task.sequence
            )


levelOrTileset : File.File -> Task.Task String ( String, File )
levelOrTileset file =
    File.toString file
        |> Task.andThen
            (\data ->
                case
                    D.decodeString
                        (D.oneOf
                            [ Tiled.Level.decode |> D.map Level
                            , Tiled.Tileset.decodeFile -1000 |> D.map Tileset
                            ]
                        )
                        data
                        |> Result.mapError D.errorToString
                of
                    Ok i ->
                        Task.succeed ( File.name file, i )

                    Err i ->
                        Task.fail i
            )


imgUrl : File.File -> String -> Task.Task String ( String, File )
imgUrl file mime =
    File.toBytes file
        |> Task.andThen
            (Base64.fromBytes
                >> Maybe.map
                    ((++) ("data:" ++ mime ++ ";base64,")
                        >> Image
                        >> Tuple.pair (File.name file)
                        >> Task.succeed
                    )
                >> Maybe.withDefault (Task.fail "Base64.fromBytes")
            )
