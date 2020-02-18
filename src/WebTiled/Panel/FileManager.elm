module WebTiled.Panel.FileManager exposing (view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import RemoteStorage
import WebTiled.Message exposing (Message(..))


view files =
    table [ class "table-striped" ]
        [ thead []
            [ tr []
                [ th []
                    [ text "Name" ]

                --, th []
                --    [ text "Kind" ]
                , th []
                    [ text "Action" ]
                ]
            ]
        , files
            |> List.map
                (\name ->
                    tr [ class "no-delay" ]
                        [ td [ onClick (GetFileRemoteStorage name) ]
                            (if String.endsWith "/" name then
                                [ span [ class "icon icon-folder" ] [], text ((++) " " <| String.replace "/" "" name) ]

                             else
                                [ text name ]
                            )

                        --, td []
                        --    [ text "unknown" ]
                        , td []
                            [ span [ class "icon icon-download" ] []
                            , span [ class "icon icon-trash" ] []
                            ]
                        ]
                )
            |> tbody []
        ]
