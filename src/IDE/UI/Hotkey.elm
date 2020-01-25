module IDE.UI.Hotkey exposing (Hotkey, Model, decode, init)

import Browser.Events as Browser
import Dict exposing (Dict)
import Json.Decode as D


type alias Model action =
    Dict Hotkey action


type alias Hotkey =
    String


init : Dict k v
init =
    Dict.empty


decode : Dict String a -> D.Decoder a
decode model =
    D.field "repeat" D.bool
        |> D.andThen
            (\repeat ->
                if repeat then
                    D.fail ""

                else
                    D.map5
                        toHotkey
                        (D.field "code" D.string)
                        (D.field "ctrlKey" D.bool)
                        (D.field "altKey" D.bool)
                        (D.field "shiftKey" D.bool)
                        (D.field "metaKey" D.bool)
                        |> D.andThen
                            (Maybe.map
                                (\hotkey ->
                                    let
                                        _ =
                                            Debug.log "aaa" hotkey
                                    in
                                    case Dict.get hotkey model of
                                        Just action ->
                                            D.succeed action

                                        Nothing ->
                                            D.fail ""
                                )
                                >> Maybe.withDefault (D.fail "")
                            )
            )


toHotkey : String -> Bool -> Bool -> Bool -> Bool -> Maybe String
toHotkey code ctrlKey altKey shiftKey metaKey =
    Dict.get code keysDict
        |> Maybe.map
            (\key ->
                [ key ]
                    |> addIf metaKey '⌘'
                    |> addIf shiftKey '⇧'
                    |> addIf altKey '⌥'
                    |> addIf ctrlKey '⌃'
                    |> String.fromList
            )


addIf : Bool -> b -> List b -> List b
addIf bool c =
    if bool then
        (::) c

    else
        identity



--ctrl+alt+Sift+cmd


keysDict : Dict String Char
keysDict =
    Dict.fromList
        [ ( "KeyA", 'A' )
        , ( "KeyB", 'B' )
        , ( "KeyC", 'C' )
        , ( "KeyD", 'D' )
        , ( "KeyE", 'E' )
        , ( "KeyF", 'F' )
        , ( "KeyG", 'G' )
        , ( "KeyH", 'H' )
        , ( "KeyI", 'I' )
        , ( "KeyJ", 'J' )
        , ( "KeyK", 'K' )
        , ( "KeyL", 'L' )
        , ( "KeyM", 'M' )
        , ( "KeyN", 'N' )
        , ( "KeyO", 'O' )
        , ( "KeyP", 'P' )
        , ( "KeyQ", 'Q' )
        , ( "KeyR", 'R' )
        , ( "KeyS", 'S' )
        , ( "KeyT", 'T' )
        , ( "KeyU", 'U' )
        , ( "KeyV", 'V' )
        , ( "KeyW", 'W' )
        , ( "KeyX", 'X' )
        , ( "KeyY", 'Y' )
        , ( "KeyZ", 'Z' )
        , ( "Digit0", '0' )
        , ( "Digit1", '1' )
        , ( "Digit2", '2' )
        , ( "Digit3", '3' )
        , ( "Digit4", '4' )
        , ( "Digit5", '5' )
        , ( "Digit6", '6' )
        , ( "Digit7", '7' )
        , ( "Digit8", '8' )
        , ( "Digit9", '9' )
        , ( "Comma", ',' )
        , ( "Period", '.' )
        , ( "Semicolon", ';' )
        , ( "Escape", '⎋' )
        , ( "Space", '␣' )
        ]



{-

   ⌘ is command 
   ⌥ is option 
   ⌃ is control 
   ⇧ is shift 
   ⇪ is caps lock 
   ← is left arrow 
   → is right arrow 
   ↑ is up arrow 
   ↓ is down arrow 
   ⇥ is tab 
   ⇤ is backtab 
   ↩ is return 
   ⌤ is enter 
   ⌫ is delete 
   ⌦ is forward delete 
   ⇞ is page up 
   ⇟ is page down 
   ↖ is home 
   ↘ is end 
   ⌧ is clear 
   ␣ is space 
   ⎋ is escape 
   ⏏ is eject

-}
