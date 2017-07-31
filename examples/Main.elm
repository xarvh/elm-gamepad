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
    { buttonMaps : Dict String Gamepad.ButtonMap
    , state : State
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )
    | OnRemapMsg Gamepad.Remap.Msg
    | OnStartRemapping
    | OnContinue
    | OnKey Keyboard.KeyCode



-- Mapping


controlsForASpecificProgram =
    [ ( LeftUp, "Move Up" )
    , ( LeftDown, "Move Down" )
    , ( LeftLeft, "Move Left" )
    , ( LeftRight, "Move Right" )
    , ( RightTrigger, "Fire" )
    , ( LeftTrigger, "Alternate Fire" )
    ]


allMappableControls =
    [ ( A, "Button A / Cross" )
    , ( B, "Button B / Circle" )
    , ( X, "Button X / Square" )
    , ( Y, "Button Y / Triangle" )
    , ( Start, "Button Start" )
    , ( Back, "Button Back / Select" )
    , ( Guide, "Guide / Logo / Home" )
    , ( LeftLeft, "Left Stick: Push Left" )
    , ( LeftRight, "Left Stick: Push Right" )
    , ( LeftUp, "Left Stick: Push Up" )
    , ( LeftDown, "Left Stick: Push Down" )
    , ( LeftStick, "Left Stick: Click" )
    , ( LeftShoulder, "Left Shoulder Button" )
    , ( LeftTrigger, "Left Trigger / Left Analog Lever" )
    , ( RightLeft, "Right Stick: Push Left" )
    , ( RightRight, "Right Stick: Push Right" )
    , ( RightUp, "Right Stick: Push Up" )
    , ( RightDown, "Right Stick: Push Down" )
    , ( RightStick, "Right Stick: Click" )
    , ( RightShoulder, "Right Shoulder Button" )
    , ( RightTrigger, "Right Trigger / Right Analog Lever" )
    , ( DpadUp, "Digital Pad Up" )
    , ( DpadDown, "Digital Pad Down" )
    , ( DpadLeft, "Digital Pad Left" )
    , ( DpadRight, "Digital Pad Right" )
    ]


controlsToMap =
    allMappableControls



-- init


init : String -> ( Model, Cmd Msg )
init gamepadCustomMapsAsString =
    noCmd
        { buttonMaps = Gamepad.buttonMapsFromString gamepadCustomMapsAsString |> Result.withDefault Dict.empty
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

        Configured gamepadId buttonMap ->
            let
                newMaps =
                    Dict.insert gamepadId buttonMap model.buttonMaps

                newMapsAsString =
                    Gamepad.buttonMapsToString newMaps

                cmd =
                    LocalStoragePort.set "gamepadCustomMaps" newMapsAsString

                newModel =
                    { model | state = Message "Successfully configured", buttonMaps = newMaps }
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
    case Gamepad.getGamepad model.buttonMaps blob 0 of
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
                        [ vc Gamepad.aIsPressed "A"
                        , vc Gamepad.bIsPressed "B"
                        , vc Gamepad.xIsPressed "X"
                        , vc Gamepad.yIsPressed "Y"
                        , vc Gamepad.startIsPressed "Start"
                        , vc Gamepad.backIsPressed "Back"
                        , vc Gamepad.guideIsPressed "Guide"
                        , vc Gamepad.dpadX "Dpad X"
                        , vc Gamepad.dpadY "Dpad Y"
                        , vc Gamepad.leftX "Left X"
                        , vc Gamepad.leftY "Left Y"
                        , vc Gamepad.leftStickIsPressed "Left Stick"
                        , vc Gamepad.leftShoulderIsPressed "Left Shoulder"
                        , vc Gamepad.leftTriggerIsPressed "Left Trigger (digital)"
                        , vc Gamepad.leftTriggerValue "Left Trigger (analog)"
                        , vc Gamepad.rightX "Right X"
                        , vc Gamepad.rightY "Right Y"
                        , vc Gamepad.rightStickIsPressed "Right Stick"
                        , vc Gamepad.rightShoulderIsPressed "Right Shoulder"
                        , vc Gamepad.rightTriggerIsPressed "Right Trigger (digital)"
                        , vc Gamepad.rightTriggerValue "Right Trigger (analog)"
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
                        [ text "(Press SPACE if you don't have this button)" ]
                    ]

            Display maybeGamepadsBlob ->
                div
                    []
                    [ div
                        []
                        [ text <| toString (Dict.size model.buttonMaps) ++ " custom gamepad maps" ]
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
