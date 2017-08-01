module Gamepad.Remap
    exposing
        ( MappableControl(..)
        , Outcome(..)
        , Model
        , Msg
        , currentEntry
        , gamepadIndex
        , skipCurrentEntry
        , init
        , update
        , view
        , subscriptions
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


type Outcome entry
    = StillOpen (Model entry)
    | Error String
    | UpdateDatabase (Gamepad.Database -> Result String Gamepad.Database)


type alias ConfiguredEntry =
    { destination : MappableControl
    , origin : Gamepad.Origin
    }


type InputState
    = WaitingForAllButtonsUp
    | WaitingForAnyButtonDown


type alias ModelRecord entry =
    { configuredEntries : List ConfiguredEntry
    , inputState : InputState
    , gamepadIndex : Int
    , maybeGamepad : Maybe Gamepad.UnknownGamepad
    , unconfiguredEntries : List ( MappableControl, entry )
    }


type Model entry
    = Model (ModelRecord entry)


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



-- init


init : Int -> List ( MappableControl, entry ) -> Model entry
init gamepadIndex entries =
    Model
        { configuredEntries = []
        , unconfiguredEntries = entries
        , inputState = WaitingForAllButtonsUp
        , gamepadIndex = gamepadIndex
        , maybeGamepad = Nothing
        }



-- update


configuredEntriesToOutcome : UnknownGamepad -> List ConfiguredEntry -> Outcome a
configuredEntriesToOutcome unknownGamepad configuredEntries =
    let
        entryToTuple entry =
            ( mappableControlToDestinationCode entry.destination, entry.origin )

        map =
            configuredEntries
                |> List.map entryToTuple
                |> Dict.fromList

        updateDatabase database =
            Gamepad.insertButtonMapInDatabase unknownGamepad map database
    in
        UpdateDatabase updateDatabase


skipCurrentEntry : Model entry -> Outcome entry
skipCurrentEntry (Model model) =
    case ( model.unconfiguredEntries, model.maybeGamepad ) of
        ( _, Nothing ) ->
            StillOpen (Model model)

        -- This should not happen, but we can recover without loss of consistency
        ( [], Just unknownGamepad ) ->
            configuredEntriesToOutcome unknownGamepad model.configuredEntries

        ( currentEntry :: remainingEntries, Just unknownGamepad ) ->
            -- Just ditch the current entry
            if remainingEntries == [] then
                configuredEntriesToOutcome unknownGamepad model.configuredEntries
            else
                StillOpen <| Model { model | unconfiguredEntries = remainingEntries, inputState = WaitingForAllButtonsUp }


onButtonPress : Gamepad.Origin -> ModelRecord entry -> Outcome entry
onButtonPress origin model =
    case ( model.unconfiguredEntries, model.maybeGamepad ) of
        ( _, Nothing ) ->
            StillOpen (Model model)

        -- This should not happen, but we can recover without loss of consistency
        ( [], Just unknownGamepad ) ->
            configuredEntriesToOutcome unknownGamepad model.configuredEntries

        ( currentEntry :: remainingEntries, Just unknownGamepad ) ->
            let
                entryConfig =
                    { destination = Tuple.first currentEntry
                    , origin = origin
                    }

                configuredEntries =
                    entryConfig :: model.configuredEntries
            in
                if remainingEntries == [] then
                    configuredEntriesToOutcome unknownGamepad configuredEntries
                else
                    StillOpen <|
                        Model
                            { model
                                | configuredEntries = configuredEntries
                                , unconfiguredEntries = remainingEntries
                            }


onMaybePressedButton : Maybe Gamepad.Origin -> ModelRecord entry -> Outcome entry
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
            let
                maybeGamepad =
                    blob
                        |> Gamepad.getUnknownGamepads Gamepad.emptyDatabase
                        |> List.filter (\unknownGamepad -> Gamepad.unknownGetIndex unknownGamepad == model.gamepadIndex)
                        |> List.head
            in
                case maybeGamepad of
                    Nothing ->
                        notConnectedError model.gamepadIndex

                    Just unknownGamepad ->
                        onMaybePressedButton (Gamepad.estimateOrigin unknownGamepad) { model | maybeGamepad = Just unknownGamepad }



-- view


gamepadIndex : Model entry -> Int
gamepadIndex (Model model) =
    model.gamepadIndex


currentEntry : Model entry -> Maybe entry
currentEntry (Model model) =
    List.head model.unconfiguredEntries |> Maybe.map Tuple.second


view : Model String -> String
view model =
    currentEntry model |> Maybe.withDefault ""



-- subscriptions


type alias PortSubscription msg =
    (( Time, Gamepad.Blob ) -> msg) -> Sub msg


subscriptions : PortSubscription Msg -> Sub Msg
subscriptions portSubscription =
    portSubscription OnGamepad
