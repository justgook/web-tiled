port module RemoteStorage exposing (getFile, getFiles, storeFile, subscriptions)

import Dict
import Json.Decode as D
import Json.Encode as E
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


subscriptions : Sub (Result Message Message)
subscriptions =
    fromStore
        (D.decodeValue (D.oneOf [ decoder2, decoder ])
            >> Result.mapError (D.errorToString >> FileError)
        )



--|>


decoder =
    D.andThen
        (\id ->
            case id of
                "list" ->
                    D.field "data" decodeListing
                        |> D.map RemoteStorageFileList

                _ ->
                    D.fail "Unknown Key"
        )
        (D.field "id" D.string)


decoder2 : D.Decoder Message
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
