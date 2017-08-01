module Main exposing (..)

import Gamepad exposing (Gamepad, UnknownGamepad)
import Gamepad.Remap exposing (MappableControl(..), Outcome(..))
import GamepadPort
import Html exposing (..)
import Html.Attributes
import Html.Events
import Keyboard
import LocalStoragePort
import Time exposing (Time)


type alias RemapModel =
    Gamepad.Remap.Model String


type State
    = Message String
    | Remapping RemapModel
    | Display (Maybe Gamepad.Blob)


type alias Model =
    { gamepadDatabase : Gamepad.Database
    , gamepadDatabaseKey : String
    , state : State
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )
    | OnRemapMsg Gamepad.Remap.Msg
    | OnStartRemapping Int
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


type alias Flags =
    { gamepadDatabaseAsString : String
    , gamepadDatabaseKey : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        gamepadDatabase =
            flags.gamepadDatabaseAsString
                |> Gamepad.databaseFromString
                |> Result.withDefault Gamepad.emptyDatabase
    in
        noCmd
            { gamepadDatabase = gamepadDatabase
            , gamepadDatabaseKey = flags.gamepadDatabaseKey
            , state = Display Nothing
            }



-- update


error message model =
    noCmd { model | state = Message <| "Error: " ++ message }


updateRemap : Gamepad.Remap.Outcome String -> Model -> ( Model, Cmd Msg )
updateRemap remapOutcome model =
    case remapOutcome of
        StillOpen remapModel ->
            noCmd { model | state = Remapping remapModel }

        Error message ->
            error message model

        UpdateDatabase updateDatabase ->
            case updateDatabase model.gamepadDatabase of
                Err message ->
                    error message model

                Ok gamepadDatabase ->
                    let
                        cmd =
                            gamepadDatabase
                                |> Gamepad.databaseToString
                                |> LocalStoragePort.set model.gamepadDatabaseKey

                        newModel =
                            { model
                                | state = Message "Successfully configured"
                                , gamepadDatabase = gamepadDatabase
                            }
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

        ( OnStartRemapping gamepadIndex, _ ) ->
            noCmd { model | state = Remapping <| Gamepad.Remap.init gamepadIndex controlsToMap }

        ( OnContinue, _ ) ->
            noCmd { model | state = Display Nothing }

        ( _, _ ) ->
            noCmd model



-- view


viewInput ( name, value ) =
    li
        []
        [ text <| name ++ "  " ++ value ]


remapButton index =
    button
        [ Html.Events.onClick (OnStartRemapping index) ]
        [ text "Remap" ]


viewControl gamepad getter name =
    let
        value =
            getter gamepad |> toString
    in
        li
            []
            [ text <| name ++ ": " ++ value ]


viewGamepad : Gamepad -> ( Int, Html Msg )
viewGamepad gamepad =
    let
        index =
            Gamepad.getIndex gamepad

        vc =
            viewControl gamepad
    in
        ( index
        , div
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
                [ remapButton index ]
            ]
        )


viewUnknownGamepad : UnknownGamepad -> ( Int, Html Msg )
viewUnknownGamepad unknownGamepad =
    let
        index =
            Gamepad.unknownGetIndex unknownGamepad
    in
        ( index
        , div
            []
            [ text "I don't know any mapping for this gamepad, but you can remap it."
            , remapButton index
            ]
        )


viewGamepadsBlob : Model -> Gamepad.Blob -> Html Msg
viewGamepadsBlob model blob =
    let
        views =
            [ Gamepad.getGamepads model.gamepadDatabase blob |> List.map viewGamepad
            , Gamepad.getUnknownGamepads model.gamepadDatabase blob |> List.map viewUnknownGamepad
            ]
                |> List.concat
                |> List.sortBy Tuple.first
                |> List.map Tuple.second
    in
        if List.length views > 0 then
            div [] views
        else
            text "No gamepads detected."


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
                        [ text "Press SPACE if you don't have this button" ]
                    , div
                        []
                        [ text "Press ESC to abort" ]
                    ]

            Display maybeGamepadsBlob ->
                div
                    []
                    [ div
                        []
                        []
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
                Gamepad.Remap.subscriptions GamepadPort.gamepad |> Sub.map OnRemapMsg

            _ ->
                Sub.none
        ]



-- main


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
