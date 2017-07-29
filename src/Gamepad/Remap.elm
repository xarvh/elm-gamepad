module Gamepad.Remap
    exposing
        ( MappableControl(..)
        , Outcome(..)
        , Model
        , Msg
        , init
        , update
        , view
        , currentEntry
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
    | LeftX
    | LeftY
    | RightX
    | RightY
    | DpadUp
    | DpadDown
    | DpadLeft
    | DpadRight


type Outcome entry
    = StillOpen (Model entry)
    | Disconnected
    | Aborted
    | Configured String Gamepad.CustomMap


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

        LeftX ->
            destinationCodes.leftX

        LeftY ->
            destinationCodes.leftY

        RightX ->
            destinationCodes.rightX

        RightY ->
            destinationCodes.rightY

        DpadUp ->
            destinationCodes.dpadUp

        DpadDown ->
            destinationCodes.dpadDown

        DpadLeft ->
            destinationCodes.dpadLeft

        DpadRight ->
            destinationCodes.dpadRight


configuredEntriesToCustomMap : List ConfiguredEntry -> Result String Gamepad.CustomMap
configuredEntriesToCustomMap entries =
    let
        entryToTuple entry =
            ( mappableControlToDestinationCode entry.destination, entry.origin )
    in
        entries
            |> List.map entryToTuple
            |> Dict.fromList
            |> Gamepad.customMap



-- init


init : Int -> List ( MappableControl, entry ) -> Model entry
init gamepadIndex entries =
    Model
        { configuredEntries = []
        , inputState = WaitingForAllButtonsUp
        , gamepadIndex = gamepadIndex
        , unconfiguredEntries = entries
        }



-- update


onButtonPress : String -> Gamepad.Origin -> Model_ entry -> Outcome entry
onButtonPress gamepadId origin model =
    case model.unconfiguredEntries of
        [] ->
            case configuredEntriesToCustomMap model.configuredEntries of
                Err s ->
                    Debug.crash "booooo!"

                Ok customMap ->
                    Configured gamepadId customMap

        currentEntry :: remainingEntries ->
            let
                entryConfig =
                    { destination = Tuple.first currentEntry
                    , origin = origin
                    }
            in
                StillOpen <|
                    Model
                        { model
                            | configuredEntries = entryConfig :: model.configuredEntries
                            , unconfiguredEntries = remainingEntries
                        }


onMaybePressedButton : String -> Maybe Gamepad.Origin -> Model_ entry -> Outcome entry
onMaybePressedButton gamepadId maybeOrigin model =
    case ( model.inputState, maybeOrigin ) of
        ( WaitingForAllButtonsUp, Just origin ) ->
            StillOpen <| Model model

        ( WaitingForAllButtonsUp, Nothing ) ->
            StillOpen <| Model { model | inputState = WaitingForAnyButtonDown }

        ( WaitingForAnyButtonDown, Just origin ) ->
            onButtonPress gamepadId origin { model | inputState = WaitingForAllButtonsUp }

        ( WaitingForAnyButtonDown, Nothing ) ->
            StillOpen <| Model model


update : Msg -> Model entry -> Outcome entry
update msg (Model model) =
    case msg of
        OnGamepad ( dt, blob ) ->
            case Gamepad.blobToRawGamepad model.gamepadIndex blob of
                Nothing ->
                    Disconnected

                Just rawGamepad ->
                    if not rawGamepad.connected then
                        Disconnected
                    else
                        onMaybePressedButton (Gamepad.getId rawGamepad) (Gamepad.estimateOrigin rawGamepad) model



-- view


currentEntry : Model entry -> Maybe entry
currentEntry (Model model) =
    List.head model.unconfiguredEntries
        |> Maybe.map Tuple.second


view : Model String -> String
view model =
    currentEntry model
        |> Maybe.withDefault "Controller configured. Press any button to continue!"



-- subscriptions


type alias PortSubscription msg =
    (( Time, Gamepad.Blob ) -> msg) -> Sub msg


subscriptions : PortSubscription Msg -> Model entry -> Sub Msg
subscriptions portSubscription model =
    Sub.batch
        [ portSubscription OnGamepad

        -- TODO: listen to keyboard and abort on Esc
        ]
