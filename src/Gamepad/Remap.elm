module Gamepad.Remap
    exposing
        ( MappableControl(..)
          -- Elm Architecture
        , Outcome(..)
        , Model
        , Msg
        , init
        , update
        , view
        , subscriptions
          -- utility
        , getCurrentButton
        , getTargetGamepadIndex
        , skipCurrentButton
        )

import Dict exposing (Dict)
import Gamepad exposing (destinationCodes, UnknownGamepad)
import Time exposing (Time)


-- types


type MappableControl
    = A
    | B
    | X
    | Y
    | Start
    | Back
    | Guide
    | LeftLeft
    | LeftRight
    | LeftUp
    | LeftDown
    | LeftStick
    | LeftShoulder
    | LeftTrigger
    | RightLeft
    | RightRight
    | RightUp
    | RightDown
    | RightStick
    | RightShoulder
    | RightTrigger
    | DpadUp
    | DpadDown
    | DpadLeft
    | DpadRight


type Outcome presentation
    = StillOpen (Model presentation)
    | Error String
    | UpdateDatabase (Gamepad.Database -> Gamepad.Database)


type alias ConfiguredEntry =
    { destination : MappableControl
    , origin : Gamepad.Origin
    }


type InputState
    = WaitingForAllButtonsUp
    | WaitingForAnyButtonDown


type alias UnconfiguredButtons presentation =
    List ( MappableControl, presentation )


type alias ModelRecord presentation =
    { configuredButtons : List ConfiguredEntry
    , inputState : InputState
    , targetUnknownGamepad : UnknownGamepad
    , unconfiguredButtons : UnconfiguredButtons presentation
    }


type Model presentation
    = Ready (ModelRecord presentation)
    | WaitingForGamepad Int (UnconfiguredButtons presentation)


type Msg
    = OnGamepad ( Time, Gamepad.Blob )



-- transforming configured buttons into a configuration string


mappableControlToDestinationCode : MappableControl -> String
mappableControlToDestinationCode mappableControl =
    case mappableControl of
        A ->
            destinationCodes.a

        B ->
            destinationCodes.b

        X ->
            destinationCodes.x

        Y ->
            destinationCodes.y

        Start ->
            destinationCodes.start

        Back ->
            destinationCodes.back

        Guide ->
            destinationCodes.guide

        LeftLeft ->
            destinationCodes.leftLeft

        LeftRight ->
            destinationCodes.leftRight

        LeftUp ->
            destinationCodes.leftUp

        LeftDown ->
            destinationCodes.leftDown

        LeftStick ->
            destinationCodes.leftStick

        LeftShoulder ->
            destinationCodes.leftShoulder

        LeftTrigger ->
            destinationCodes.leftTrigger

        RightLeft ->
            destinationCodes.rightLeft

        RightRight ->
            destinationCodes.rightRight

        RightUp ->
            destinationCodes.rightUp

        RightDown ->
            destinationCodes.rightDown

        RightStick ->
            destinationCodes.rightStick

        RightShoulder ->
            destinationCodes.rightShoulder

        RightTrigger ->
            destinationCodes.rightTrigger

        DpadUp ->
            destinationCodes.dpadUp

        DpadDown ->
            destinationCodes.dpadDown

        DpadLeft ->
            destinationCodes.dpadLeft

        DpadRight ->
            destinationCodes.dpadRight



-- helpers


indexToUnknownGamepad : Gamepad.Blob -> Int -> Maybe UnknownGamepad
indexToUnknownGamepad blob index =
    let
        isTargetGamepad unknownGamepad =
            Gamepad.unknownGetIndex unknownGamepad == index
    in
        blob
            |> Gamepad.getUnknownGamepads Gamepad.emptyDatabase
            |> List.filter isTargetGamepad
            |> List.head


notConnectedError : Int -> Outcome presentation
notConnectedError gamepadIndex =
    Error <| "Gamepad " ++ toString gamepadIndex ++ " is not connected"



-- init


init : Int -> UnconfiguredButtons presentation -> Model presentation
init gamepadIndex buttonsToConfigure =
    WaitingForGamepad gamepadIndex buttonsToConfigure


actuallyInit : Gamepad.Blob -> Int -> UnconfiguredButtons presentation -> Outcome presentation
actuallyInit blob gamepadIndex buttonsToConfigure =
    case indexToUnknownGamepad blob gamepadIndex of
        Nothing ->
            notConnectedError gamepadIndex

        Just targetUnknownGamepad ->
            { configuredButtons = []
            , unconfiguredButtons = buttonsToConfigure
            , inputState = WaitingForAllButtonsUp
            , targetUnknownGamepad = targetUnknownGamepad
            }
                |> Ready
                |> StillOpen



-- update


configuredButtonsToOutcome : UnknownGamepad -> List ConfiguredEntry -> Outcome a
configuredButtonsToOutcome targetUnknownGamepad configuredButtons =
    let
        configuredButtonToTuple button =
            ( mappableControlToDestinationCode button.destination, button.origin )

        map =
            configuredButtons
                |> List.map configuredButtonToTuple
                |> Dict.fromList
    in
        case Gamepad.buttonMapToUpdateDatabase targetUnknownGamepad map of
            Err message ->
                Error message

            Ok updateDatabase ->
                UpdateDatabase updateDatabase


skipCurrentButton : Model presentation -> Outcome presentation
skipCurrentButton unionModel =
    case unionModel of
        WaitingForGamepad gamepadIndex buttonsToConfigure ->
            StillOpen unionModel

        Ready recordModel ->
            case recordModel.unconfiguredButtons of
                -- This should not happen, but we can recover without loss of consistency
                [] ->
                    configuredButtonsToOutcome recordModel.targetUnknownGamepad recordModel.configuredButtons

                currentButton :: remainingButton ->
                    -- Just ditch the current button
                    if remainingButton == [] then
                        configuredButtonsToOutcome recordModel.targetUnknownGamepad recordModel.configuredButtons
                    else
                        { recordModel
                            | unconfiguredButtons = remainingButton
                            , inputState = WaitingForAllButtonsUp
                        }
                            |> Ready
                            |> StillOpen


onButtonPress : Gamepad.Origin -> ModelRecord presentation -> Outcome presentation
onButtonPress origin model =
    case model.unconfiguredButtons of
        -- This should not happen, but we can recover without loss of consistency
        [] ->
            configuredButtonsToOutcome model.targetUnknownGamepad model.configuredButtons

        currentButton :: remainingButton ->
            let
                buttonConfig =
                    { destination = Tuple.first currentButton
                    , origin = origin
                    }

                configuredButtons =
                    buttonConfig :: model.configuredButtons
            in
                if remainingButton == [] then
                    configuredButtonsToOutcome model.targetUnknownGamepad configuredButtons
                else
                    { model
                        | configuredButtons = configuredButtons
                        , unconfiguredButtons = remainingButton
                    }
                        |> Ready
                        |> StillOpen


onMaybePressedButton : Maybe Gamepad.Origin -> ModelRecord presentation -> Outcome presentation
onMaybePressedButton maybeOrigin model =
    case ( model.inputState, maybeOrigin ) of
        ( WaitingForAllButtonsUp, Just origin ) ->
            model
                |> Ready
                |> StillOpen

        ( WaitingForAllButtonsUp, Nothing ) ->
            { model | inputState = WaitingForAnyButtonDown }
                |> Ready
                |> StillOpen

        ( WaitingForAnyButtonDown, Just origin ) ->
            onButtonPress origin { model | inputState = WaitingForAllButtonsUp }

        ( WaitingForAnyButtonDown, Nothing ) ->
            model
                |> Ready
                |> StillOpen


update : Msg -> Model presentation -> Outcome presentation
update msg unionModel =
    case msg of
        OnGamepad ( dt, blob ) ->
            case unionModel of
                WaitingForGamepad index unconfiguredButtons ->
                    actuallyInit blob index unconfiguredButtons

                Ready model ->
                    -- fetch the new state of the target gamepad
                    case indexToUnknownGamepad blob (Gamepad.unknownGetIndex model.targetUnknownGamepad) of
                        Nothing ->
                            notConnectedError (Gamepad.unknownGetIndex model.targetUnknownGamepad)

                        Just targetUnknownGamepad ->
                            onMaybePressedButton (Gamepad.estimateOrigin targetUnknownGamepad) { model | targetUnknownGamepad = targetUnknownGamepad }



-- view


getTargetGamepadIndex : Model presentation -> Int
getTargetGamepadIndex unionModel =
    case unionModel of
        Ready recordModel ->
            Gamepad.unknownGetIndex recordModel.targetUnknownGamepad

        WaitingForGamepad gamepadIndex buttonsToConfigure ->
            gamepadIndex


getCurrentButton : Model presentation -> Maybe presentation
getCurrentButton unionModel =
    case unionModel of
        Ready recordModel ->
            List.head recordModel.unconfiguredButtons |> Maybe.map Tuple.second

        WaitingForGamepad gamepadIndex buttonsToConfigure ->
            Nothing


view : Model String -> String
view model =
    getCurrentButton model |> Maybe.withDefault ""



-- subscriptions


type alias PortSubscription msg =
    (( Time, Gamepad.Blob ) -> msg) -> Sub msg


subscriptions : PortSubscription Msg -> Sub Msg
subscriptions portSubscription =
    portSubscription OnGamepad