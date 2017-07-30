module Main exposing (..)

import Array
import Dict exposing (Dict)
import Gamepad
import Gamepad.Remap exposing (MappableControl(..), Outcome(..))
import GamepadPort
import Html exposing (..)
import Html.Attributes
import Html.Events
import Keyboard
import LocalStoragePort
import Time exposing (Time)


type State
    = Message String
    | Remapping (Gamepad.Remap.Model String)
    | DisplayInputs (List ( String, String ))


type alias Model =
    { customMaps : Dict String Gamepad.CustomMap
    , state : State
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )
    | OnRemapMsg Gamepad.Remap.Msg
    | OnStartRemapping
    | OnContinue
    | OnKey Keyboard.KeyCode



-- Mapping


controlsToMap =
    [ ( LeftUp, "up" )
    , ( LeftDown, "down" )
    , ( LeftLeft, "left" )
    , ( LeftRight, "right" )
    ]



-- init


init : String -> ( Model, Cmd Msg )
init gamepadCustomMapsAsString =
    noCmd
        { customMaps = Gamepad.customMapsFromString gamepadCustomMapsAsString |> Result.withDefault Dict.empty
        , state = DisplayInputs []
        }



-- update


updateRemap : Gamepad.Remap.Outcome String -> Model -> ( Model, Cmd Msg )
updateRemap remapOutcome model =
    case remapOutcome of
        StillOpen remapModel ->
            noCmd { model | state = Remapping remapModel }

        Error message ->
            noCmd { model | state = Message <| "Error: " ++ message }

        Configured gamepadId customMap ->
            let
                newMaps =
                    Dict.insert gamepadId customMap model.customMaps

                newMapsAsString =
                    Gamepad.customMapsToString newMaps

                cmd =
                    LocalStoragePort.set "gamepadCustomMaps" newMapsAsString

                newModel =
                    { model | state = Message "Successfully configured", customMaps = newMaps }
            in
                ( newModel, cmd )


updateInputDisplay : Gamepad.Blob -> Model -> Model
updateInputDisplay gamepadsBlob model =
    case Gamepad.getGamepad model.customMaps gamepadsBlob 0 of
        Gamepad.Disconnected ->
            { model | state = Message "Gamepad 1 is disconnected" }

        Gamepad.Unrecognised ->
            { model | state = Message "I don't know any mapping for Gamepad 1, but you can configure it!" }

        Gamepad.Available gamepad ->
            let
                inputs =
                    [ ( "woot", "yeah" ) ]
            in
                { model | state = DisplayInputs inputs }


noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.state ) of
        ( OnRemapMsg remapMsg, Remapping remapModel ) ->
            updateRemap (Gamepad.Remap.update remapMsg remapModel) model

        ( OnKey keyCode, Remapping remapModel ) ->
            case keyCode of
                -- Esc: abort remapping
                27 ->
                    noCmd { model | state = DisplayInputs [] }

                -- Space: skip current entry
                32 ->
                    updateRemap (Gamepad.Remap.skipCurrentEntry remapModel) model

                _ ->
                    noCmd model

        ( OnGamepad ( time, gamepadsBlob ), DisplayInputs _ ) ->
            noCmd <| updateInputDisplay gamepadsBlob model

        ( OnStartRemapping, _ ) ->
            noCmd { model | state = Remapping <| Gamepad.Remap.init 0 controlsToMap }

        ( OnContinue, _ ) ->
            noCmd { model | state = DisplayInputs [] }

        ( _, _ ) ->
            noCmd model



-- view


viewInput ( name, value ) =
    li
        []
        [ text <| name ++ "  " ++ value ]


remapButton =
    button
        [ Html.Events.onClick OnStartRemapping ]
        [ text "Remap" ]


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ text <| toString (Dict.size model.customMaps) ++ " custom gamepad maps" ]
        , case model.state of
            Message message ->
                div
                    []
                    [ div
                        []
                        [ text message ]
                    , div
                        []
                        [ button
                            [ Html.Events.onClick OnContinue ]
                            [ text "Go back to display" ]
                        ]
                    ]

            Remapping remapModel ->
                div [] [ text <| Gamepad.Remap.view remapModel ]

            DisplayInputs inputs ->
                div
                    []
                    [ ul
                        []
                        (List.map viewInput inputs)
                    , div
                        []
                        [ remapButton ]
                    ]
        ]



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ GamepadPort.gamepad OnGamepad
        , Keyboard.ups OnKey
        , case model.state of
            Remapping remapModel ->
                Gamepad.Remap.subscriptions GamepadPort.gamepad remapModel |> Sub.map OnRemapMsg

            _ ->
                Sub.none
        ]



-- main


main : Program String Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
