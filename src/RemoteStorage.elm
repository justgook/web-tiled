port module RemoteStorage exposing (connect, disconnect, getFile, getFiles, saveLevel, storeFile, subscriptions)

import Dict
import Json.Decode as D
import Json.Encode as E
import Tiled.Level as Tiled
import WebTiled.Message exposing (Message(..))


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


connect : String -> Cmd msg
connect userAddress =
    toStore
        (E.object
            [ ( "method", E.string "connect" )
            , ( "id", E.string "connect" )
            , ( "params", E.list identity [ E.string userAddress ] )
            ]
        )


disconnect : Cmd msg
disconnect =
    toStore
        (E.object
            [ ( "method", E.string "disconnect" )
            , ( "id", E.string "disconnect" )
            , ( "params", E.list identity [] )
            ]
        )


saveLevel : String -> Tiled.Level -> List ( String, String ) -> List ( String, String ) -> Cmd msg
saveLevel levelPath level images tilesets =
    tilesets
        |> List.foldl
            (\( path, content ) ->
                (::) (storeFile "application/tiled.tileset-json" path content)
            )
            (List.foldl (\( path, content ) -> (::) (storeFile "image/base64" path content))
                [ Tiled.encode level
                    |> E.encode 0
                    |> storeFile "application/tiled.level-json" levelPath
                ]
                images
            )
        >> Cmd.batch


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


subscriptions : Sub Message
subscriptions =
    fromStore (D.decodeValue (D.oneOf [ decoder2, decoder ]) >> Result.mapError (D.errorToString >> FileError))
        |> Sub.map
            (\result ->
                case result of
                    Ok msg ->
                        msg

                    Err msg ->
                        msg
            )


decoder : D.Decoder Message
decoder =
    D.field "id" D.string
        |> D.andThen
            (\id ->
                case id of
                    "list" ->
                        D.field "data" decodeListing
                            |> D.map RemoteStorageFileList

                    "not-connected" ->
                        D.succeed RemoteStorageOffline

                    "connected" ->
                        D.at [ "data", "userAddress" ] D.string
                            |> D.map RemoteStorageOnline

                    "wire-busy" ->
                        D.succeed RemoteStorageSyncing

                    "sync-done" ->
                        D.succeed RemoteStorageSyncDone

                    key ->
                        RemoteStorageUnhandledEvent key
                            |> D.succeed
            )


decoder2 : D.Decoder Message
decoder2 =
    D.field "id" (D.map2 (\id name -> ( id, name )) (D.index 0 D.string) (D.index 1 D.string))
        |> D.andThen
            (\( id, name ) ->
                case id of
                    "getFile" ->
                        D.oneOf [ D.field "data" (decodeGotFile name), D.succeed (RemoteStorageFileMissing name) ]

                    _ ->
                        D.fail "Unknown Key"
            )


decodeGotFile : String -> D.Decoder Message
decodeGotFile name =
    D.map2 (RemoteStorageFile name)
        (D.field "contentType" D.string)
        (D.field "data" D.string)


decodeListing : D.Decoder (List String)
decodeListing =
    D.map2 (\private public -> Dict.union private public |> Dict.keys)
        (D.field "private" decodeFiles)
        (D.field "public" decodeFiles)


decodeFiles : D.Decoder (Dict.Dict String (Maybe a))
decodeFiles =
    D.dict (D.succeed Nothing)
