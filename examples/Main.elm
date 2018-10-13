module Main exposing (..)

import Browser
import Gamepad exposing (Gamepad)
import GamepadPort
import Html exposing (..)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import LocalStoragePort


-- Keyboard


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- types


type State
    = Remapping Gamepad.RemapModel -- This means that we are remapping a gamepad
    | Display (Maybe Gamepad.Blob) -- This means that we are displaying the gamepads mapped controls


type alias Model =
    { userMappings : Gamepad.UserMappings
    , state : State
    }


type Msg
    = OnGamepad Gamepad.Blob
    | OnRemapMsg Gamepad.RemapMsg
    | OnToggleRemap



-- Mapping


{-| Most of the times, we want to remap only the controls that our application
will actually use, and name them according to the function they will have for
the application.
-}
controlsForASpecificProgram =
    [ ( "Move Up", Gamepad.LeftStickUp )
    , ( "Move Down", Gamepad.LeftStickDown )
    , ( "Move Left", Gamepad.LeftStickLeft )
    , ( "Move Right", Gamepad.LeftStickRight )
    , ( "Fire", Gamepad.RightTrigger )
    , ( "Jump", Gamepad.LeftTrigger )
    ]


{-| Since this specific example/ can be used also for testing, I think it
is useful to have a complete list of controls with the names of the physical
buttons rather than the name of their effect for a specific application.
-}
allMappableControls =
    [ ( "Button A / Cross", Gamepad.A )
    , ( "Button B / Circle", Gamepad.B )
    , ( "Button X / Square", Gamepad.X )
    , ( "Button Y / Triangle", Gamepad.Y )
    , ( "Button Start", Gamepad.Start )
    , ( "Button Back / Select", Gamepad.Back )
    , ( "Logo / Home / Guide", Gamepad.Home )
    , ( "Left Stick: Push Left", Gamepad.LeftStickLeft )
    , ( "Left Stick: Push Right", Gamepad.LeftStickRight )
    , ( "Left Stick: Push Up", Gamepad.LeftStickUp )
    , ( "Left Stick: Push Down", Gamepad.LeftStickDown )
    , ( "Left Stick: Click", Gamepad.LeftStickPress )
    , ( "Left Bumper Button", Gamepad.LeftBumper )
    , ( "Left Trigger / Left Analog Lever", Gamepad.LeftTrigger )
    , ( "Right Stick: Push Left", Gamepad.RightStickLeft )
    , ( "Right Stick: Push Right", Gamepad.RightStickRight )
    , ( "Right Stick: Push Up", Gamepad.RightStickUp )
    , ( "Right Stick: Push Down", Gamepad.RightStickDown )
    , ( "Right Stick: Click", Gamepad.RightStickPress )
    , ( "Right Bumper Button", Gamepad.RightBumper )
    , ( "Right Trigger / Right Analog Lever", Gamepad.RightTrigger )
    , ( "Directional Pad Up", Gamepad.DpadUp )
    , ( "Directional Pad Down", Gamepad.DpadDown )
    , ( "Directional Pad Left", Gamepad.DpadLeft )
    , ( "Directional Pad Right", Gamepad.DpadRight )
    ]


controlsToMap =
    allMappableControls



-- init


type alias Flags =
    { userMappingsAsString : String
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        userMappings =
            flags.userMappingsAsString
                |> Gamepad.userMappingsFromString
                |> Result.withDefault Gamepad.emptyUserMappings
    in
    noCmd
        { userMappings = userMappings
        , state = Display Nothing
        }



-- update


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


type alias UpdateUserMappings =
    Gamepad.UserMappings -> Gamepad.UserMappings


updateOnRemapMsg : ( Gamepad.RemapModel, Maybe UpdateUserMappings ) -> Model -> ( Model, Cmd Msg )
updateOnRemapMsg ( remapModel, maybeUpdateUserMappings ) model =
    updateOnMappings maybeUpdateUserMappings { model | state = Remapping remapModel }


updateOnMappings : Maybe UpdateUserMappings -> Model -> ( Model, Cmd a )
updateOnMappings maybeUpdateUserMappings model =
    case maybeUpdateUserMappings of
        Nothing ->
            noCmd model

        Just updateMappings ->
            let
                newUserMappings =
                    updateMappings model.userMappings

                newModel =
                    { model | userMappings = newUserMappings }

                cmd =
                    newUserMappings
                        |> Gamepad.userMappingsToString
                        |> LocalStoragePort.set "userMappings"
            in
            ( newModel, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- How we interprete each Msg depends on the current state of the app,
    -- so we consider the two together.
    case ( msg, model.state ) of
        ( OnRemapMsg remapMsg, Remapping remapModel ) ->
            updateOnRemapMsg (Gamepad.remapUpdate controlsToMap remapMsg remapModel) model

        -- Gamepad input is used only when we are in `Display` status.
        -- (Gamepad.Remap has its own subscription to get gamepad data).
        ( OnGamepad gamepadsBlob, Display _ ) ->
            noCmd  { model | state = Display (Just gamepadsBlob) }

        ( OnToggleRemap, _ ) ->
            noCmd
                { model
                    | state =
                        case model.state of
                            Remapping _ ->
                                Display Nothing

                            _ ->
                                Remapping Gamepad.remapInit
                }

        ( _, _ ) ->
            noCmd model



-- view


boolToString : Bool -> String
boolToString bool =
    case bool of
        True ->
            "True"

        False ->
            "False"


recordToString : { x : Float, y : Float } -> String
recordToString { x, y } =
    "{ x: " ++ String.fromFloat x ++ ", y: " ++ String.fromFloat y ++ " }"


dpadToString : { x : Int, y : Int } -> String
dpadToString { x, y } =
    String.fromInt x ++ "," ++ String.fromInt y


viewDigital : Gamepad -> ( String, Gamepad.Digital ) -> Html msg
viewDigital gamepad ( name, digital ) =
    li
        []
        [ boolToString (Gamepad.isPressed gamepad digital) ++ " <- " ++ name |> text ]


viewAnalog : String -> Html msg
viewAnalog string =
    li [] [ text string ]


viewGamepad : Gamepad -> Html Msg
viewGamepad gamepad =
    div
        []
        [ h3
            []
            [ "Gamepad " ++ String.fromInt (Gamepad.getIndex gamepad) |> text ]
        , allMappableControls
            |> List.map (viewDigital gamepad)
            |> ul []
        , [ "Left Stick postion: " ++ recordToString (Gamepad.leftStickPosition gamepad)
          , "Right Stick position: " ++ recordToString (Gamepad.rightStickPosition gamepad)
          , "Dpad position: " ++ dpadToString (Gamepad.dpadPosition gamepad)
          , "Left Trigger (analog)) :" ++ String.fromFloat (Gamepad.value gamepad Gamepad.RightTriggerAnalog)
          , "Right Trigger (analog)) :" ++ String.fromFloat (Gamepad.value gamepad Gamepad.RightTriggerAnalog)
          ]
            |> List.map viewAnalog
            |> ul []
        ]


viewGamepadsBlob : Model -> Gamepad.Blob -> Html Msg
viewGamepadsBlob model blob =
    let
        views =
            Gamepad.getGamepads model.userMappings blob |> List.map viewGamepad
    in
    if List.length views > 0 then
        div [] views
    else
        text "No gamepads detected."


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Gamepad example"
    , body =
        case model.state of
            Remapping remapModel ->
                [ Gamepad.remapView controlsToMap model.userMappings remapModel Nothing |> Html.map OnRemapMsg
                , div [] []
                , div [] []
                , button [ Html.Events.onClick OnToggleRemap ] [ text "Close Remap Tool" ]
                ]

            Display maybeGamepadsBlob ->
                [ div [] []
                , button [ Html.Events.onClick OnToggleRemap ] [ text "Open Remap Tool" ]
                , div
                    []
                    [ case maybeGamepadsBlob of
                        Nothing ->
                            text "Waiting..."

                        Just gamepadsBlob ->
                            viewGamepadsBlob model gamepadsBlob
                    ]
                ]
    }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ GamepadPort.gamepad OnGamepad

        -- TODO, Browser.onDocument "keyup" (Decode.map OnKey keyDecoder)
        , case model.state of
            Remapping remapModel ->
                Gamepad.remapSubscriptions GamepadPort.gamepad |> Sub.map OnRemapMsg

            _ ->
                Sub.none
        ]



-- main


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
