module WebTiled.Panel.Toolbar.Run exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy
import WebTiled.Message exposing (Message(..))


view build =
    header [ class "toolbar toolbar-header" ]
        [ div [ class "toolbar-actions" ]
            [ Html.Lazy.lazy css ()
            , div [ class "btn-group" ]
                [ button [ class "btn btn-positive", onClick Run ]
                    [ div [ class "run-menu-button" ] [ span [ class "icon icon-play" ] [], text build.selected.name ]
                    ]
                , button [ tabindex -1, class "btn btn-positive", style "padding" "3px 0" ]
                    [ span [ class "icon icon-down-open" ] []
                    , ul [ class "drop-down-menu" ]
                        (List.indexedMap (\i { name } -> li [ onClick (SetRunScript i) ] [ text name ]) build.rest)
                    ]
                ]
            ]
        ]


css : () -> Html msg
css _ =
    [ text """

.run-menu-button {
  white-space: nowrap;
  width: 70px;
  overflow: hidden;
  text-overflow: ellipsis;
}
.drop-down-menu * {list-style: none;}
.drop-down-menu  {
    padding:0;
    margin:0;
    text-align: left;
    color: #333;
    border: 1px solid #bebebe;
    border-radius: 6px;
    box-sizing:border-box;
    min-width: 150px;
    padding-bottom: 3px;
    padding-top: 3px;
    position: absolute;
    top: 24px;
    left: -5px;
    z-index: 4;
    background: #f5f5f4;
    font-size: 14px;
}
.drop-down-menu li {
    padding-left: 10px;
    height: 22px;
    display: flex;
    flex-wrap: nowrap;
    white-space: nowrap;
    justify-content: space-between;
    align-items: center;
}
button:focus > .drop-down-menu li:hover,
.drop-down-menu li:active {
    background-color: #2865cd;
    color: #fff;
}

button > .drop-down-menu {
    transition: visibility 0s linear 0.2s;
    visibility: hidden;
}

button:focus > .drop-down-menu {
    transition-delay: 0s;
    visibility: visible;
}
""" ]
        |> Html.node "style" []
