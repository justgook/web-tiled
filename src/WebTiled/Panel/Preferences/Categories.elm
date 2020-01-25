module WebTiled.Panel.Preferences.Categories exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import WebTiled.Message exposing (Message(..), PreferencesTab(..))


view model =
    nav [ class "nav-group" ]
        [ h5 [ class "nav-group-title" ] [ text "Preferences" ]
        , navItem "icon-user" "Account" model Account
        , navItem "icon-cog" "General" model General
        , navItem "icon-window" "Appearance" model Appearance
        , navItem "icon-keyboard" "Keyboard" model Keyboard
        , navItem "icon-cd" "Radial Menu" model RadialMenu
        , navItem "icon-mobile" "Mobile Settings" model MobileSettings
        , navItem "icon-rocket" "Publish" model Publish
        , navItemAttention "icon-tools" "Build Server" model BuildServer
        , navItemAlert "icon-back-in-time" "Backup" model Backup
        ]


navItem : String -> String -> PreferencesTab -> PreferencesTab -> Html Message
navItem icon name current action =
    a [ onClick (ShowPreferences action), class "nav-group-item", classList [ ( "active", current == action ) ] ]
        [ span [ class "icon", class icon ] []
        , text name
        ]


navItemAttention : String -> String -> PreferencesTab -> PreferencesTab -> Html Message
navItemAttention icon name current action =
    a [ onClick (ShowPreferences action), class "nav-group-item", classList [ ( "active", current == action ) ] ]
        [ span [ class "icon", class icon ] []
        , text name
        , span [ class "icon icon-attention", style "float" "right", style "color" "#fece72" ] []
        ]


navItemAlert : String -> String -> PreferencesTab -> PreferencesTab -> Html Message
navItemAlert icon name current action =
    a [ onClick (ShowPreferences action), class "nav-group-item", classList [ ( "active", current == action ) ] ]
        [ span [ class "icon", class icon ] []
        , text name
        , span [ class "icon icon-alert ", style "float" "right", style "color" "#fb1710" ] []
        ]
