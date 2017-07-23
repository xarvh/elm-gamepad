module Main exposing (..)

import Array
import Gamepad
import GamepadPort
import Html exposing (..)
import Time exposing (Time)


type alias Model =
    { time : Time
    , blob : Maybe Gamepad.Blob
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )



--


noCmd model =
    ( model, Cmd.none )


init =
    noCmd { time = 0, blob = Nothing }


update msg model =
    case msg of
        OnGamepad ( time, blob ) ->
            noCmd { time = time, blob = Just blob }



--


viewGamepad : Gamepad.Connection -> Html msg
viewGamepad connection =
    case connection of
        Gamepad.Disconnected ->
            text "disconnected"

        Gamepad.Unrecognised ->
            text "not recognised"

        Gamepad.Available pad ->
            [ ( "a", Gamepad.aIsPressed >> toString )
            , ( "b", Gamepad.bIsPressed >> toString )
            , ( "x", Gamepad.xIsPressed >> toString )
            , ( "y", Gamepad.yIsPressed >> toString )

            --
            , ( "start", Gamepad.startIsPressed >> toString )
            , ( "back", Gamepad.backIsPressed >> toString )
            , ( "guide", Gamepad.guideIsPressed >> toString )

            --
            , ( "leftX", Gamepad.leftX >> toString )
            , ( "leftY", Gamepad.leftY >> toString )
            , ( "rightX", Gamepad.rightX >> toString )
            , ( "rightY", Gamepad.rightY >> toString )
            ]
                |> List.map (\( s, f ) -> div [] [ text s, text ": ", text <| f pad ])
                |> div []


showRaw raw =
    raw.buttons
        |> Array.toList
        |> List.indexedMap (\index b -> div [] [ text <| (toString index) ++ " " ++ (toString b) ])
        |> div []


view model =
    case model.blob of
        Nothing ->
            text ""

        Just blob ->
            div
                []
                [ List.range 0 3
                    |> List.map (Gamepad.getGamepad blob)
                    |> List.map viewGamepad
                    |> List.map (\h -> li [] [ h ])
                    |> ul []
                , blob
                    |> List.filterMap identity
                    |> List.map showRaw
                    |> div []
                ]



--


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = \model -> GamepadPort.gamepad OnGamepad
        , view = view
        }
