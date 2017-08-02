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


-- types


{-| Gamepad.Remap.Model's argument is the type describe how to present
each button that we want to map.
Since we just want to display the text name of each button, a String will do.
-}
type alias RemapModel =
    Gamepad.Remap.Model String


type State
    = Message String -- Just display a message
    | Remapping RemapModel -- This means that we are remapping a gamepad
    | Display (Maybe Gamepad.Blob) -- This means that we are displaying the gamepads mapped controls


type alias Model =
    { gamepadDatabase : Gamepad.Database
    , gamepadDatabaseKey : String -- This is the key we use for the database in the browser's local storage
    , state : State
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )
    | OnRemapMsg Gamepad.Remap.Msg
    | OnStartRemapping Int
    | OnContinue
    | OnKey Keyboard.KeyCode



-- Mapping


{-| Most of the times, we want to remap only the controls that our application
will actually use, and name them according to the function they will have for
the application.
-}
controlsForASpecificProgram =
    [ ( LeftUp, "Move Up" )
    , ( LeftDown, "Move Down" )
    , ( LeftLeft, "Move Left" )
    , ( LeftRight, "Move Right" )
    , ( RightTrigger, "Fire" )
    , ( LeftTrigger, "Alternate Fire" )
    ]


{-| Since this specific example/ can be used also for testing, I think it
is useful to have a complete list of controls with the names of the physical
buttons rather than the name of their effect for a specific application.
-}
allMappableControls =
    [ ( A, "Button A / Cross" )
    , ( B, "Button B / Circle" )
    , ( X, "Button X / Square" )
    , ( Y, "Button Y / Triangle" )
    , ( Start, "Button Start" )
    , ( Back, "Button Back / Select" )
    , ( Home, "Logo / Home / Guide" )
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


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


error : String -> Model -> ( Model, Cmd msg )
error message model =
    noCmd { model | state = Message <| "Error: " ++ message }


{-| The update function for Gamepad.Remap does not return its Model, but rather
a type telling the parent what to do next.
-}
updateRemap : Gamepad.Remap.Outcome String -> Model -> ( Model, Cmd Msg )
updateRemap remapOutcome model =
    case remapOutcome of
        -- This means that the remapping is still in progress.
        StillOpen remapModel ->
            noCmd { model | state = Remapping remapModel }

        -- This means that something went wrong with the remapping process.
        -- Usually it means that the gamepad was disconnected.
        Error message ->
            error message model

        -- This means that the user is done remapping.
        -- `updateDatabse` is the function to use to actually insert the
        -- new button map inside the gamepad database.
        UpdateDatabase updateDatabase ->
            let
                gamepadDatabase =
                    updateDatabase model.gamepadDatabase

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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- How we interprete each Msg depends on the current state of the app,
    -- so we consider the two together.
    case ( msg, model.state ) of
        ( OnRemapMsg remapMsg, Remapping remapModel ) ->
            updateRemap (Gamepad.Remap.update remapMsg remapModel) model

        -- Keys are used only when remapping
        ( OnKey keyCode, Remapping remapModel ) ->
            case keyCode of
                -- Esc: abort remapping
                27 ->
                    noCmd { model | state = Display Nothing }

                -- Space: skip current entry
                32 ->
                    updateRemap (Gamepad.Remap.skipCurrentButton remapModel) model

                _ ->
                    noCmd model

        -- Gamepad input is used only when we are in `Display` status.
        -- (Gamepad.Remap has its own subscription to get gamepad data).
        ( OnGamepad ( time, gamepadsBlob ), Display _ ) ->
            noCmd <| { model | state = Display (Just gamepadsBlob) }

        ( OnStartRemapping gamepadIndex, _ ) ->
            noCmd { model | state = Remapping <| Gamepad.Remap.init gamepadIndex controlsToMap }

        ( OnContinue, _ ) ->
            noCmd { model | state = Display Nothing }

        ( _, _ ) ->
            noCmd model



-- view


viewRemapButton : Int -> Html Msg
viewRemapButton index =
    button
        [ Html.Events.onClick (OnStartRemapping index) ]
        [ text "Remap" ]


viewGamepad : Gamepad -> ( Int, Html Msg )
viewGamepad gamepad =
    let
        index =
            Gamepad.getIndex gamepad

        viewControl : (Gamepad -> a) -> String -> Html msg
        viewControl getter name =
            li
                []
                [ text <| name ++ ": " ++ toString (getter gamepad) ]
    in
        ( index
        , div
            []
            [ ul
                []
                [ viewControl Gamepad.aIsPressed "A"
                , viewControl Gamepad.bIsPressed "B"
                , viewControl Gamepad.xIsPressed "X"
                , viewControl Gamepad.yIsPressed "Y"
                , viewControl Gamepad.startIsPressed "Start"
                , viewControl Gamepad.backIsPressed "Back"
                , viewControl Gamepad.homeIsPressed "Home"
                , viewControl Gamepad.dpadX "Dpad X"
                , viewControl Gamepad.dpadY "Dpad Y"
                , viewControl Gamepad.leftX "Left X"
                , viewControl Gamepad.leftY "Left Y"
                , viewControl Gamepad.leftStickIsPressed "Left Stick"
                , viewControl Gamepad.leftShoulderIsPressed "Left Shoulder"
                , viewControl Gamepad.leftTriggerIsPressed "Left Trigger (digital)"
                , viewControl Gamepad.leftTriggerValue "Left Trigger (analog)"
                , viewControl Gamepad.rightX "Right X"
                , viewControl Gamepad.rightY "Right Y"
                , viewControl Gamepad.rightStickIsPressed "Right Stick"
                , viewControl Gamepad.rightShoulderIsPressed "Right Shoulder"
                , viewControl Gamepad.rightTriggerIsPressed "Right Trigger (digital)"
                , viewControl Gamepad.rightTriggerValue "Right Trigger (analog)"
                ]
            , div
                []
                [ viewRemapButton index ]
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
            , viewRemapButton index
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
                        [ text <| "> " ++ Gamepad.Remap.view remapModel ++ " <" ]
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
