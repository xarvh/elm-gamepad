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
    | Display (Maybe Gamepad.Blob)


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
    [ ( LeftUp, "Move Up" )
    , ( LeftDown, "Move Down" )
    , ( LeftLeft, "Move Left" )
    , ( LeftRight, "Move Right" )
    , ( RightTrigger, "Fire" )
    , ( LeftTrigger, "Alternate Fire" )
    ]



-- init


init : String -> ( Model, Cmd Msg )
init gamepadCustomMapsAsString =
    noCmd
        { customMaps = Gamepad.customMapsFromString gamepadCustomMapsAsString |> Result.withDefault Dict.empty
        , state = Display Nothing
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
                    noCmd { model | state = Display Nothing }

                -- Space: skip current entry
                32 ->
                    updateRemap (Gamepad.Remap.skipCurrentEntry remapModel) model

                _ ->
                    noCmd model

        ( OnGamepad ( time, gamepadsBlob ), Display _ ) ->
            noCmd <| { model | state = Display (Just gamepadsBlob) }

        ( OnStartRemapping, _ ) ->
            noCmd { model | state = Remapping <| Gamepad.Remap.init 0 controlsToMap }

        ( OnContinue, _ ) ->
            noCmd { model | state = Display Nothing }

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


viewControl gamepad getter name =
    let
        value =
            getter gamepad |> toString
    in
        li
            []
            [ text <| name ++ ": " ++ value ]


viewGamepadsBlob : Model -> Gamepad.Blob -> Html Msg
viewGamepadsBlob model blob =
    case Gamepad.getGamepad model.customMaps blob 0 of
        Gamepad.Disconnected ->
            text "disconnected"

        Gamepad.Unrecognised ->
            div
                []
                [ text "I don't know any mapping for this gamepad, but you can remap it."
                , remapButton
                ]

        Gamepad.Available gamepad ->
            let
                vc =
                    viewControl gamepad
            in
                div
                    []
                    [ ul
                        []
                        [ vc Gamepad.leftX "Move X"
                        , vc Gamepad.leftY "Move Y"
                        , vc Gamepad.leftTriggerValue "Alternate Fire (analog)"
                        , vc Gamepad.leftTriggerIsPressed "Alternate Fire (digital)"
                        , vc Gamepad.rightTriggerValue "Fire (analog)"
                        , vc Gamepad.rightTriggerIsPressed "Fire (digital)"
                        ]
                    , div
                        []
                        [ remapButton ]
                    ]


view : Model -> Html Msg
view model =
    div
        []
        [ case model.state of
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
                div
                    []
                    [ div
                        []
                        [ text "Press the button you want to use for:" ]
                    , div
                        []
                        [ text <| "----> " ++ Gamepad.Remap.view remapModel ++ " <----" ]
                    , div
                        []
                        [ text "(Press SPACE to skip)" ]
                    ]

            Display maybeGamepadsBlob ->
                div
                    []
                    [ div
                        []
                        [ text <| toString (Dict.size model.customMaps) ++ " custom gamepad maps" ]
                    , div
                        []
                        [ case maybeGamepadsBlob of
                            Nothing ->
                                text "Waiting..."

                            Just gamepadsBlob ->
                                viewGamepadsBlob model gamepadsBlob
                        ]
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
