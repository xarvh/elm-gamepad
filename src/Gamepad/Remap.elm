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
    | Configured String


type alias UnconfiguredEntry =
    { destination : MappableControl
    , label : String
    }


type alias ConfiguredEntry =
    { destination : MappableControl
    , originCode : String
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


configuredEntriesToGamepadConfigString : List ConfiguredEntry -> String
configuredEntriesToGamepadConfigString entries =
    let
        entryToConfig entry =
            mappableControlToDestinationCode entry.destination ++ ":" ++ entry.originCode
    in
        entries
            |> List.map entryToConfig
            |> String.join ","



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


onButtonPress : String -> Model -> Outcome
onButtonPress originCode model =
    case model.unconfiguredEntries of
        [] ->
            Configured <| configuredEntriesToGamepadConfigString model.configuredEntries

        currentEntry :: remainingEntries ->
            let
                entryConfig =
                    { destination = currentEntry.destination
                    , originCode = originCode
                    }
            in
                StillOpen
                    { model
                        | configuredEntries = entryConfig :: model.configuredEntries
                        , unconfiguredEntries = remainingEntries
                    }


onMaybePressedButton : Time -> Maybe String -> Model -> Outcome
onMaybePressedButton dt maybeOriginCode model =
    case ( model.inputState, maybeOriginCode ) of
        ( WaitingForAllButtonsUp, Just originCode ) ->
            StillOpen model

        ( WaitingForAllButtonsUp, Nothing ) ->
            StillOpen { model | inputState = WaitingForAnyButtonDown }

        ( WaitingForAnyButtonDown, Just originCode ) ->
            onButtonPress originCode { model | inputState = WaitingForAllButtonsUp }

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
                        noCmd <| onMaybePressedButton dt (Gamepad.estimateOriginCode rawGamepad) model



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
