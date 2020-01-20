module WebTiled.Panel.FileManager exposing (Model, init, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import RemoteStorage


type alias Model =
    ()


init : Model
init =
    ()



--view : Model -> Dict String File -> Dict String (Maybe File) -> Html (Model -> Model)


view m files inStore =
    table [ class "table-striped" ]
        [ thead []
            [ tr []
                [ th []
                    [ text "Name" ]
                , th []
                    [ text "Kind" ]
                , th []
                    [ text "Action" ]
                ]
            ]
        , Dict.foldl
            (\name v ->
                (::) <|
                    tr [ onClick (loadFile name), class "no-delay" ]
                        [ td []
                            (if String.endsWith "/" name then
                                [ span [ class "icon icon-folder" ] [], text name ]

                             else
                                [ text name ]
                            )
                        , td []
                            [ text "image" ]
                        , td []
                            [ span [ class "icon icon-download" ] []
                            , span [ class "icon icon-trash" ] []
                            ]
                        ]
            )
            []
            inStore
            |> tbody []
        ]


loadFile : String -> b -> ( b, Cmd msg )
loadFile name e =
    ( e, RemoteStorage.getFile name )
