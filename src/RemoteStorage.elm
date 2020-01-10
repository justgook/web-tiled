port module RemoteStorage exposing (getFile, getFiles, storeFile, subscriptions)

import Dict
import Json.Decode as D
import Json.Encode as E
import Tiled.Level
import WebTiled.DropFiles exposing (File(..))


port toStore : E.Value -> Cmd msg


port fromStore : (D.Value -> msg) -> Sub msg


getFiles : Cmd msg
getFiles =
    toStore
        (E.object
            [ ( "method", E.string "getFiles" )
            , ( "id", E.string "list" )
            , ( "params", E.list identity [] )
            ]
        )


storeFile : String -> String -> String -> Cmd msg
storeFile mimeType path body =
    toStore
        (E.object
            [ ( "method", E.string "storeFile" )
            , ( "id", E.string "storeFile" )
            , ( "params", E.list E.string [ mimeType, path, body ] )
            ]
        )


getFile : String -> Cmd msg
getFile path =
    toStore
        (E.object
            [ ( "method", E.string "getFile" )
            , ( "id", E.list E.string [ "getFile", path ] )
            , ( "params", E.list E.string [ path ] )
            ]
        )


subscriptions =
    D.oneOf [ decoder, decoder2 ]
        |> D.decodeValue
        |> fromStore


decoder =
    D.andThen
        (\id ->
            case id of
                "list" ->
                    D.field "data" decodeListing

                _ ->
                    D.fail "Unknown Key"
        )
        (D.field "id" D.string)


decoder2 =
    D.field "id" (D.map2 (\id name -> ( id, name )) (D.index 0 D.string) (D.index 1 D.string))
        |> D.andThen
            (\( id, name ) ->
                case id of
                    "getFile" ->
                        D.field "data" (decodeGotFile name)

                    _ ->
                        D.fail "Unknown Key"
            )


decodeGotFile name =
    D.field "contentType" D.string
        |> D.andThen
            (\contentType ->
                case contentType of
                    "application/tiled.level-json" ->
                        D.map
                            (\data editor ->
                                case D.decodeString Tiled.Level.decode data of
                                    Ok l ->
                                        { editor | files = Dict.insert name (Level l) editor.files }

                                    Err _ ->
                                        editor
                            )
                            (D.field "data" D.string)

                    "image/base64" ->
                        D.map
                            (\data editor -> { editor | files = Dict.insert name (Image data) editor.files })
                            (D.field "data" D.string)

                    _ ->
                        D.fail ""
            )


decodeListing =
    D.map2 (\private public editor -> { editor | inStore = Dict.union private public })
        (D.field "private" decodeFiles)
        (D.field "public" decodeFiles)


decodeFiles : D.Decoder (Dict.Dict String (Maybe a))
decodeFiles =
    D.dict (D.succeed Nothing)
