module Main exposing (..)

import Array
import Dict
import Gamepad
import Gamepad.Remap exposing (MappableControl(..), Outcome(..))
import GamepadPort
import Html exposing (..)
import Html.Attributes as HA
import Time exposing (Time)


type alias Model =
    { time : Time
    , blob : Maybe Gamepad.Blob
    , remapOutcome : Gamepad.Remap.Outcome
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )
    | OnRemapMsg Gamepad.Remap.Msg



-- Mapping


inputNames =
    [ ( LeftUp, "up" )
    , ( LeftDown, "down" )
    , ( LeftLeft, "left" )
    , ( LeftRight, "right" )
    ]



-- init


init =
    noCmd
        { time = 0
        , blob = Nothing
        , remapOutcome = StillOpen <| Gamepad.Remap.init 0 inputNames
        }



-- update


noCmd model =
    ( model, Cmd.none )


update msg model =
    case msg of
        OnGamepad ( time, blob ) ->
            noCmd { model | blob = Just blob }

        OnRemapMsg nestedMsg ->
            case model.remapOutcome of
                StillOpen nestedModel ->
                    noCmd { model | remapOutcome = Gamepad.Remap.update nestedMsg nestedModel }

                _ ->
                    noCmd model



-- view


viewGamepad : Gamepad.Connection -> Html msg
viewGamepad connection =
    case connection of
        Gamepad.Disconnected ->
            text "disconnected"

        Gamepad.Unrecognised ->
            text "not recognised"

        Gamepad.Available pad ->
            let
                ts ( name, getter ) =
                    toString name ++ ": " ++ toString (getter pad)
            in
                [ ts ( "X", Gamepad.leftX )
                , ts ( "Y", Gamepad.leftY )
                ]
                    |> List.map (\s -> div [] [ text s ])
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
            case model.remapOutcome of
                Gamepad.Remap.StillOpen nestedModel ->
                    text <| Gamepad.Remap.view nestedModel

                Gamepad.Remap.Configured gamepadId customMap ->
                    let
                        q =
                            Debug.log "--" customMap

                        customMaps =
                            Dict.fromList [ ( gamepadId, customMap ) ]
                    in
                        viewGamepad <| Gamepad.getGamepad customMaps blob 0

                _ ->
                    text "Meh"



-- subscriptions


subscriptions model =
    Sub.batch
        [ GamepadPort.gamepad OnGamepad
        , case model.remapOutcome of
            StillOpen nestedModel ->
                Gamepad.Remap.subscriptions GamepadPort.gamepad nestedModel |> Sub.map OnRemapMsg

            _ ->
                Sub.none
        ]



-- main


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
