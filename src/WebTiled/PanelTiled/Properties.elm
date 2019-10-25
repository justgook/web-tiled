module WebTiled.PanelTiled.Properties exposing (customProps, propertiesTable, propertyRow)

import Dict
import Html exposing (Attribute, Html, input, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, style, type_)
import IDE.UI.Widget as Widget
import Tiled.Properties exposing (Properties, Property(..))


propertiesTable ( mainText, props1 ) props2 =
    [ table [ class "table-striped" ]
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
                    [ text "Custom Properties" ]
                ]
            ]
        , tbody [] props2
        ]
    ]


customProps =
    Dict.foldl (\k v acc -> propertyRow k v :: acc) []


propertyRow key prop =
    tr []
        [ td []
            [ text key ]
        , td []
            [ propToWidget prop ]
        ]


propToWidget prop =
    case prop of
        PropString value ->
            text value

        PropColor value ->
            let
                color =
                    if value == "" then
                        "#111111"

                    else
                        value
            in
            input [ type_ "color", Html.Attributes.value color ] []

        PropInt value ->
            let
                m =
                    { value = toFloat value, string = String.fromInt value, focus = True }

                _ =
                    Debug.log "PropInt" value
            in
            Widget.float m

        PropFloat value ->
            let
                m =
                    { value = value, string = String.fromFloat value, focus = True }
            in
            Widget.float m

        PropBool value ->
            input [ type_ "checkbox", Html.Attributes.checked value ] []

        _ ->
            text "propertyRow"
