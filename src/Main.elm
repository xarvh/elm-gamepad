module Main exposing (..)

import Html
import Html.App
import String
import Time exposing (Time)

import Gamepad exposing (Gamepad)



type alias Model =
    { time : Time
    , pads : List Gamepad
    }


type Msg
    = Pad (Time.Time, List Gamepad)





init =
    ( Model 0 [], Cmd.none )


update msg model =
    case msg of
        Pad (time, pads) ->
            ( Model time pads, Cmd.none )



viewGamepads gamepads =
        Html.div [] (List.map (\g -> Html.div [] [ Html.text <| toString g ]) gamepads)




view model =
    Html.div
        []
        [ Html.div [] [ Html.text (toString model.time)]
        , Html.div [] [viewGamepads model.pads ]
        ]



main =
    Html.App.program
        { init = init
        , update = update
        , subscriptions = \model -> Gamepad.animationFrameAndGamepads Pad
        , view = view
        }
