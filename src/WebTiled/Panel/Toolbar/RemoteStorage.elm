module WebTiled.Panel.Toolbar.RemoteStorage exposing (view)

import Html exposing (button, div, header, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import TypedSvg exposing (polygon, svg)
import TypedSvg.Attributes exposing (points, viewBox)
import TypedSvg.Attributes.InPx exposing (height, width)
import WebTiled.Message exposing (Message(..), PreferencesTab(..))
import WebTiled.Model exposing (RemoteStorageStatus(..))


view status =
    header [ class "toolbar toolbar-header" ]
        [ div [ class "toolbar-actions " ]
            [ button [ class "btn btn-default pull-right", onClick (ShowPreferences Account) ]
                [ span [ class "icon" ] [ icon status ]
                ]
            ]
        ]


icon status =
    svg
        [ viewBox 0 0 739 853
        , width 14
        , height 14
        , case status of
            Online ->
                style "fill" "#FF4B03"

            Syncing ->
                style "fill" "#edaf3b"

            _ ->
                style "fill" "#737475"
        ]
        [ polygon [ points [ ( 370, 754 ), ( 0, 542 ), ( 0, 640 ), ( 185, 747 ), ( 370, 853 ), ( 554, 747 ), ( 739, 640 ), ( 739, 525 ), ( 739, 525 ), ( 739, 476 ), ( 739, 427 ), ( 739, 378 ), ( 653, 427 ), ( 370, 589 ), ( 86, 427 ), ( 86, 427 ), ( 86, 361 ), ( 185, 418 ), ( 370, 524 ), ( 554, 418 ), ( 653, 361 ), ( 739, 311 ), ( 739, 213 ), ( 739, 213 ), ( 554, 107 ), ( 370, 0 ), ( 185, 107 ), ( 58, 180 ), ( 144, 230 ), ( 228, 181 ), ( 370, 100 ), ( 511, 181 ), ( 652, 263 ), ( 370, 425 ), ( 87, 263 ), ( 87, 263 ), ( 0, 213 ), ( 0, 213 ), ( 0, 311 ), ( 0, 378 ), ( 0, 427 ), ( 0, 476 ), ( 86, 525 ), ( 185, 582 ), ( 370, 689 ), ( 554, 582 ), ( 653, 525 ), ( 653, 590 ), ( 653, 592 ) ] ] [] ]
