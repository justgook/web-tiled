module WebTiled.Util.Http exposing (getBytes, getLevel)

import Base64
import Dict
import File
import Http exposing (Error(..))
import Json.Decode as D
import Task exposing (Task)
import Tiled
import Tiled.Tileset
import WebTiled.Message exposing (Message(..))
import WebTiled.Util.Tiled as TiledUtil


getLevel : String -> Cmd Message
getLevel url =
    let
        relUrl =
            String.split "/" url
                |> List.reverse
                |> List.drop 1
                |> (::) ""
                |> List.reverse
                |> String.join "/"
    in
    getJson url Tiled.decode
        |> Task.andThen
            (\level ->
                let
                    missing =
                        TiledUtil.sourceTileset level
                in
                if List.isEmpty missing then
                    Task.succeed ( level, [] )

                else
                    List.foldl
                        (\{ firstgid, source } ->
                            getJson (relUrl ++ source) (Tiled.Tileset.decodeFile firstgid)
                                |> Task.map (Tuple.pair firstgid)
                                |> (::)
                        )
                        []
                        missing
                        |> Task.sequence
                        |> Task.map (Tuple.pair level)
            )
        |> Task.attempt
            (\result ->
                case result of
                    Ok ( level, tilesets ) ->
                        FileFromUrl ( relUrl, String.replace relUrl "" url ) level tilesets

                    Err err ->
                        FileError err
            )


getJson : String -> D.Decoder a -> Task String a
getJson url decoder =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.GoodStatus_ _ body ->
                            D.decodeString decoder body
                                |> Result.mapError D.errorToString

                        Http.BadUrl_ info ->
                            Err <| "BadUrl:" ++ info

                        Http.Timeout_ ->
                            Err <| "Timeout"

                        Http.NetworkError_ ->
                            Err "NetworkError"

                        Http.BadStatus_ { statusText, statusCode } _ ->
                            Err <| "BadStatus (" ++ String.fromInt statusCode ++ "):" ++ statusText
                )
        , timeout = Nothing
        }


getBytes : String -> Task String String
getBytes url =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver =
            Http.bytesResolver
                (\response ->
                    case response of
                        Http.GoodStatus_ { headers } body ->
                            case Base64.fromBytes body of
                                Just s ->
                                    s
                                        |> (++) ";base64,"
                                        |> (++) (Dict.get "content-type" headers |> Maybe.withDefault "")
                                        |> (++) "data:"
                                        |> Ok

                                Nothing ->
                                    Err "Cannot encode bytes"

                        Http.BadUrl_ info ->
                            Err <| "BadUrl:" ++ info

                        Http.Timeout_ ->
                            Err <| "Timeout"

                        Http.NetworkError_ ->
                            Err "NetworkError"

                        Http.BadStatus_ { statusText, statusCode } _ ->
                            Err <| "BadStatus (" ++ String.fromInt statusCode ++ "):" ++ statusText
                )
        , timeout = Nothing
        }
