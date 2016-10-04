module Main exposing (..)

import Html
import Html.App
import String
import Time

import Gamepad exposing (Gamepad)



type alias Model =
    List Gamepad


type Msg
    = Pad (Time.Time, List Gamepad)





init =
    ( [], Cmd.none ) --Gamepad.gamepadAndAnimationFrame Pad )


update msg model =
    case msg of
        Pad (time, pads) ->
            ( pads, Cmd.none ) --Gamepad.gamepadAndAnimationFrame Pad )



viewGamepads gamepads =
        Html.div [] (List.map (\g -> Html.div [] [ Html.text <| toString g ]) gamepads)




view model =
    Html.div
        []
        [ Html.div [] []
        , Html.div [] [viewGamepads model ]
        ]



main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = \model -> Gamepad.animationFrameAndGamepads Pad
        , view = view
        }
