module WebTiled.Panel.Properties.Generic exposing (custom, properties, property, readOnly)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Tiled.Properties exposing (Properties, Property(..))
import WebTiled.Panel.Generic


properties : String -> Properties -> Int -> Int -> List (Html msg) -> Html msg
properties mainText props2 w h props1 =
    table [ class "table-striped" ]
        [ thead []
            [ tr []
                [ th []
                    [ text "Property" ]
                , th []
                    [ text "Value" ]
                ]
            ]
        , tbody []
            [ tr []
                [ th [ colspan 2, style "background-color" "#dcdfe1" ]
                    [ text mainText ]
                ]
            ]
        , tbody [] props1
        , tbody []
            [ tr []
                [ th [ colspan 2, style "background-color" "#dcdfe1" ]
                    [ span [ class "pull-left" ] [ text "Custom Properties" ]

                    --, div [ class "pull-right" ] [ button [ class "btn btn-mini" ] [ span [ class "icon icon-flow-tree" ] [] ] ]
                    , div [ class "btn-group pull-right" ]
                        [ button [ class "btn btn-mini btn-default" ] [ span [ class "icon icon-plus-circled" ] [] ]
                        , button [ class "btn btn-mini btn-default" ] [ span [ class "icon icon-minus-circled" ] [] ]

                        --, button [ class "btn btn-mini btn-default active" ] [ span [ class "icon icon-cloud" ] [] ]
                        , button [ class "btn btn-mini btn-default active" ] [ span [ class "icon icon-flow-cascade" ] [] ]

                        --, button [ class "btn btn-mini btn-default" ] [ span [ class "icon icon-popup" ] [] ]
                        ]
                    ]
                ]
            ]
        , tbody [] (custom props2)
        ]
        |> WebTiled.Panel.Generic.panel w h "Properties"


custom : Properties -> List (Html msg)
custom =
    Dict.foldl (\k v acc -> property k v :: acc) []


property : String -> Property -> Html msg
property key prop =
    tr []
        [ td []
            [ text key ]
        , td []
            [ propToWidget prop ]
        ]


readOnly : String -> String -> Html msg
readOnly key prop =
    tr [ class "disabled" ]
        [ td []
            [ text key ]
        , td []
            [ text prop ]
        ]


propToWidget prop =
    case prop of
        String value ->
            input [ type_ "text", Html.Attributes.value value ] []

        Color value ->
            input [ type_ "text", Html.Attributes.value value ] []

        Int value ->
            input [ type_ "text", Html.Attributes.value (String.fromInt value) ] []

        Float value ->
            input [ type_ "text", Html.Attributes.value (String.fromFloat value) ] []

        Bool value ->
            input [ type_ "checkbox", Html.Attributes.checked value ] []

        File value ->
            input [ type_ "text", Html.Attributes.value value ] []

        Object value ->
            input [ type_ "text", Html.Attributes.value (String.fromInt value) ] []
