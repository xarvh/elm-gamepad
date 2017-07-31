module Gamepad.Remap
    exposing
        ( MappableControl(..)
        , Outcome(..)
        , Model
        , Msg
        , currentEntry
        , skipCurrentEntry
        , init
        , update
        , view
        , subscriptions
        )

import Dict exposing (Dict)
import Gamepad exposing (destinationCodes)
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


type Outcome entry
    = StillOpen (Model entry)
    | Error String
    | Configured String Gamepad.ButtonMap


type alias ConfiguredEntry =
    { destination : MappableControl
    , origin : Gamepad.Origin
    }


type InputState
    = WaitingForAllButtonsUp
    | WaitingForAnyButtonDown


type alias Model_ entry =
    { configuredEntries : List ConfiguredEntry
    , inputState : InputState
    , gamepadIndex : Int
    , gamepadId : String
    , unconfiguredEntries : List ( MappableControl, entry )
    }


type Model entry
    = Model (Model_ entry)


type Msg
    = OnGamepad ( Time, Gamepad.Blob )



-- transforming configured entries into a configuration string


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


configuredEntriesToButtonMap : List ConfiguredEntry -> Result String Gamepad.ButtonMap
configuredEntriesToButtonMap entries =
    let
        entryToTuple entry =
            ( mappableControlToDestinationCode entry.destination, entry.origin )
    in
        entries
            |> List.map entryToTuple
            |> Dict.fromList
            |> Gamepad.buttonMap



-- init


init : Int -> List ( MappableControl, entry ) -> Model entry
init gamepadIndex entries =
    Model
        { configuredEntries = []
        , inputState = WaitingForAllButtonsUp
        , gamepadIndex = gamepadIndex
        , gamepadId = ""
        , unconfiguredEntries = entries
        }



-- update


configuredEntriesToOutcome : String -> List ConfiguredEntry -> Outcome a
configuredEntriesToOutcome gamepadId configuredEntries =
    case configuredEntriesToButtonMap configuredEntries of
        Err message ->
            Error "Gamepad.Remap bugged. =("

        Ok buttonMap ->
            Configured gamepadId buttonMap


skipCurrentEntry : Model entry -> Outcome entry
skipCurrentEntry (Model model) =
    case model.unconfiguredEntries of
        [] ->
            Error "This should not happen."

        currentEntry :: remainingEntries ->
            -- Just ditch the current entry
            if remainingEntries == [] then
                configuredEntriesToOutcome model.gamepadId model.configuredEntries
            else
                StillOpen <| Model { model | unconfiguredEntries = remainingEntries, inputState = WaitingForAllButtonsUp }


onButtonPress : Gamepad.Origin -> Model_ entry -> Outcome entry
onButtonPress origin model =
    case model.unconfiguredEntries of
        [] ->
            configuredEntriesToOutcome model.gamepadId model.configuredEntries

        currentEntry :: remainingEntries ->
            let
                entryConfig =
                    { destination = Tuple.first currentEntry
                    , origin = origin
                    }

                configuredEntries =
                    entryConfig :: model.configuredEntries
            in
                if remainingEntries == [] then
                    configuredEntriesToOutcome model.gamepadId configuredEntries
                else
                    StillOpen <|
                        Model
                            { model
                                | configuredEntries = configuredEntries
                                , unconfiguredEntries = remainingEntries
                            }


onMaybePressedButton : Maybe Gamepad.Origin -> Model_ entry -> Outcome entry
onMaybePressedButton maybeOrigin model =
    case ( model.inputState, maybeOrigin ) of
        ( WaitingForAllButtonsUp, Just origin ) ->
            StillOpen <| Model model

        ( WaitingForAllButtonsUp, Nothing ) ->
            StillOpen <| Model { model | inputState = WaitingForAnyButtonDown }

        ( WaitingForAnyButtonDown, Just origin ) ->
            onButtonPress origin { model | inputState = WaitingForAllButtonsUp }

        ( WaitingForAnyButtonDown, Nothing ) ->
            StillOpen (Model model)


notConnectedError : Int -> Outcome entry
notConnectedError gamepadIndex =
    Error <| "Gamepad " ++ toString gamepadIndex ++ " is not connected"


update : Msg -> Model entry -> Outcome entry
update msg (Model model) =
    case msg of
        OnGamepad ( dt, blob ) ->
            case Gamepad.blobToRawGamepad model.gamepadIndex blob of
                Nothing ->
                    notConnectedError model.gamepadIndex

                Just rawGamepad ->
                    if not rawGamepad.connected then
                        notConnectedError model.gamepadIndex
                    else
                        onMaybePressedButton (Gamepad.estimateOrigin rawGamepad) { model | gamepadId = Gamepad.rawGetId rawGamepad }



-- view


currentEntry : Model entry -> Maybe entry
currentEntry (Model model) =
    List.head model.unconfiguredEntries |> Maybe.map Tuple.second


view : Model String -> String
view model =
    currentEntry model |> Maybe.withDefault ""



-- subscriptions


type alias PortSubscription msg =
    (( Time, Gamepad.Blob ) -> msg) -> Sub msg


subscriptions : PortSubscription Msg -> Model entry -> Sub Msg
subscriptions portSubscription model =
    portSubscription OnGamepad
