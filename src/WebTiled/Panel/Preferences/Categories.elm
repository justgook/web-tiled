module WebTiled.Panel.Preferences.Categories exposing (init, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import WebTiled.Message exposing (PreferencesTab(..))


init : PreferencesTab
init =
    Keyboard


view : PreferencesTab -> Html msg
view model =
    nav [ class "nav-group" ]
        [ h5 [ class "nav-group-title" ] [ text "Preferences" ]
        , navItem "icon-user" "Account" (model == Account)
        , navItem "icon-cog" "General" (model == General)
        , navItem "icon-window" "Appearance" (model == Appearance)
        , navItem "icon-keyboard" "Keyboard" (model == Keyboard)
        , navItem "icon-cd" "Radial Menu" (model == RadialMenu)
        , navItem "icon-mobile" "Mobile Settings" (model == MobileSettings)
        , navItem "icon-rocket" "Publish" (model == Publish)
        , navItemAttention "icon-tools" "Build Server" (model == BuildServer)
        , navItemAlert "icon-back-in-time" "Backup" (model == Backup)
        ]


navItem : String -> String -> Bool -> Html msg
navItem icon name active =
    a [ class "nav-group-item", classList [ ( "active", active ) ] ]
        [ span [ class "icon", class icon ] []
        , text name
        ]


navItemAttention : String -> String -> Bool -> Html msg
navItemAttention icon name active =
    a [ class "nav-group-item", classList [ ( "active", active ) ] ]
        [ span [ class "icon", class icon ] []
        , text name
        , span [ class "icon icon-attention", style "float" "right", style "color" "#fece72" ] []
        ]


navItemAlert : String -> String -> Bool -> Html msg
navItemAlert icon name active =
    a [ class "nav-group-item", classList [ ( "active", active ) ] ]
        [ span [ class "icon", class icon ] []
        , text name
        , span [ class "icon icon-alert ", style "float" "right", style "color" "#fb1710" ] []
        ]
