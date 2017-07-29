module Gamepad.Remap
    exposing
        ( MappableControl(..)
        , Outcome(..)
        , Model
        , Msg
        , init
        , update
        , view
        , subscriptions
        )

import Dict exposing (Dict)
import Gamepad exposing (destinationCodes)
import Html exposing (..)
import List.Extra
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


type Outcome
    = StillOpen Model
    | Disconnected
    | Aborted
    | Configured String Gamepad.CustomMap


type alias UnconfiguredEntry =
    { destination : MappableControl
    , label : String
    }


type alias ConfiguredEntry =
    { destination : MappableControl
    , origin : Gamepad.Origin
    }


type InputState
    = WaitingForAllButtonsUp
    | WaitingForAnyButtonDown


type alias Model =
    { configuredEntries : List ConfiguredEntry
    , inputState : InputState
    , gamepadIndex : Int
    , unconfiguredEntries : List UnconfiguredEntry
    }


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


init : Int -> List ( MappableControl, String ) -> ( Model, Cmd Msg )
init gamepadIndex controlNames =
    let
        tupleToMapEntry ( control, label ) =
            { destination = control
            , label = label
            }

        unconfiguredEntries =
            controlNames
                |> List.Extra.uniqueBy (Tuple.first >> mappableControlToDestinationCode)
                |> List.map tupleToMapEntry
    in
        noCmd
            { configuredEntries = []
            , inputState = WaitingForAllButtonsUp
            , gamepadIndex = gamepadIndex
            , unconfiguredEntries = unconfiguredEntries
            }



-- update


onButtonPress : String -> Gamepad.Origin -> Model -> Outcome
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
                    { destination = currentEntry.destination
                    , origin = origin
                    }
            in
                StillOpen
                    { model
                        | configuredEntries = entryConfig :: model.configuredEntries
                        , unconfiguredEntries = remainingEntries
                    }


onMaybePressedButton : String -> Maybe Gamepad.Origin -> Model -> Outcome
onMaybePressedButton gamepadId maybeOrigin model =
    case ( model.inputState, maybeOrigin ) of
        ( WaitingForAllButtonsUp, Just origin ) ->
            StillOpen model

        ( WaitingForAllButtonsUp, Nothing ) ->
            StillOpen { model | inputState = WaitingForAnyButtonDown }

        ( WaitingForAnyButtonDown, Just origin ) ->
            onButtonPress gamepadId origin { model | inputState = WaitingForAllButtonsUp }

        ( WaitingForAnyButtonDown, Nothing ) ->
            StillOpen model


noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Outcome, Cmd Msg )
update msg model =
    case msg of
        OnGamepad ( dt, blob ) ->
            case Gamepad.blobToRawGamepad model.gamepadIndex blob of
                Nothing ->
                    noCmd Disconnected

                Just rawGamepad ->
                    if not rawGamepad.connected then
                        noCmd Disconnected
                    else
                        noCmd <| onMaybePressedButton (Gamepad.getId rawGamepad) (Gamepad.estimateOrigin rawGamepad) model



-- view


view : Model -> Html Msg
view model =
    case model.unconfiguredEntries of
        [] ->
            text "All done! Press any button to continue!"

        currentEntry :: remainingEntries ->
            text currentEntry.label



-- subscriptions


type alias PortSubscription msg =
    (( Time, Gamepad.Blob ) -> msg) -> Sub msg


subscriptions : PortSubscription Msg -> Model -> Sub Msg
subscriptions portSubscription model =
    Sub.batch
        [ portSubscription OnGamepad

        -- TODO: listen to keyboard and abort on Esc
        ]
