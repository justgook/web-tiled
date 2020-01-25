module IDE.UI.TopMenu exposing (MenuItem(..), view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy


view items =
    Html.Lazy.lazy css ()
        :: menu items
        |> ul [ class "menu", id "top-menu", tabindex -1 ]


type MenuItem msg
    = Separator
    | MenuItem Bool (Maybe msg) String (List (MenuItem msg))
    | Checkbox Bool String Bool


menu =
    List.map
        (\item ->
            case item of
                Separator ->
                    li [ class "menu-separator" ] []

                Checkbox enabled label checked ->
                    li [ class "menu-item checked" ]
                        [ span [ class "menu-label", classList [ ( "disabled", not enabled ) ] ]
                            [ if checked then
                                span [ class "icon icon-check" ] []

                              else
                                text ""
                            , text label
                            ]
                        ]

                MenuItem enabled msg label submenu ->
                    li [ class "menu-item" ]
                        (if List.isEmpty submenu then
                            [ a (attrs msg enabled) [ text label ] ]

                         else
                            [ span [ class "menu-label", classList [ ( "disabled", not enabled ) ] ] [ text label, span [ class "icon icon-play" ] [] ]
                            , menu submenu |> ul [ class "sub-menu" ]
                            ]
                        )
        )


attrs msg enabled =
    case msg of
        Just msg_ ->
            [ class "menu-label", onClick msg_, classList [ ( "disabled", not enabled ) ] ]

        Nothing ->
            [ class "menu-label", classList [ ( "disabled", not enabled ) ] ]


css : () -> Html msg
css _ =
    Html.node "style" [] [ text """

.menu, .sub-menu {
    margin: 0;
    padding: 0;
}
.menu {
   background: #f5f5f4;
}
.menu * {list-style: none;}
.sub-menu  {
    border: 1px solid #bebebe;
    border-radius: 6px;
    box-sizing:border-box;
    min-width: 150px;
    padding-bottom: 3px;
    padding-top: 3px;
}
.menu {
    font-size: 14px;
    display: flex;
    padding-left:10px;
    flex-flow: row no-wrap;

}
.menu > .menu-item > .menu-label > .icon {
    display: none;
}

.menu-item {
    position: relative;
    background: inherit;
}

.menu-label {
    display: flex;
    flex-wrap: nowrap;
    white-space: nowrap;
    justify-content: space-between;
    height: 22px;
    padding: 0 10px;
    background: inherit;
}

.sub-menu .menu-label {
    padding-left: 20px;
}
.menu-separator {
    height: 8px;
    border-bottom: 2px solid #cdcdcd;
    padding: 0;
    margin-bottom: 6px;
}

.menu-item .sub-menu {
    position: absolute;
    left: 0;
    z-index: 4;
    background: inherit;
}
.menu-item .icon {
    padding-left: 10px;
}
.menu-item.checked > .menu-label > .icon {
    padding-left: 5px;
    position: absolute;
    left: 0;
}
.menu > .menu-item > .sub-menu {
    border-top:none;
    border-top-left-radius: 0;
    border-top-right-radius: 0;
}
.menu > .menu-item > .sub-menu .sub-menu {
    left: 100%;
    margin-left: -1px;
    top: -4px;
}
.menu > .menu-item > .sub-menu > .menu-item:nth-child(1) > .sub-menu {
    top: 0;
}

.menu:focus .menu-item:hover > .menu-label {
    background-color: #2865cd;
    color: #fff;
}

.sub-menu {
    visibility: hidden;
}

.menu:focus .menu-item:hover .sub-menu {
    visibility: visible;
}

.menu:focus .menu-item:hover .sub-menu .sub-menu {
    visibility: hidden;
}

.menu:focus .menu-item:hover .sub-menu .menu-item:hover>.sub-menu {
    visibility: visible;
}
""" ]
