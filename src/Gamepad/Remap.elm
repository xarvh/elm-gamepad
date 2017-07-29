module Gamepad.Remap exposing (..)

import Array
import Dict exposing (Dict)
import Gamepad exposing (destinationCodes)
import Gamepad.Visual
import Html exposing (..)
import List.Extra
import Time exposing (Time)


--
-- Guess
--
-- This code is used to get an estimate of the buttons/sticks the user is
-- moving given a time series of RawGamepad states
--
-- TODO: move in its own module?


{-| Buttons are always provided as a (isPressed, value) tuple.
The function ignores one and uses only nd always the other.

Is this a good assumption?
Are there cases where both should be considered?

-}
buttonToEstimate originIndex ( isPressed, value ) =
    if isPressed then
        ( "b" ++ toString originIndex, 1 )
    else
        ( "b" ++ toString originIndex, 0 )


axisToEstimate originIndex value =
    if value < 0 then
        ( "-a" ++ toString originIndex, -value )
    else
        ( "a" ++ toString originIndex, value )


addEstimate : ( String, Float ) -> Dict String Float -> Dict String Float
addEstimate ( code, weight ) oldEstimates =
    let
        newWeight =
            oldEstimates
                |> Dict.get code
                |> Maybe.withDefault 0
                |> (+) weight
    in
        Dict.insert code newWeight oldEstimates


addRawGamepadToEstimates : Gamepad.RawGamepad -> Dict String Float
addRawGamepadToEstimates rawGamepad =
    let
        axesEstimates =
            Array.indexedMap axisToEstimate rawGamepad.axes

        buttonsEstimates =
            Array.indexedMap buttonToEstimate rawGamepad.buttons
    in
        Array.append axesEstimates buttonsEstimates
            |> Array.foldr addEstimate Dict.empty


estimateThreshold : ( String, Float ) -> Maybe String
estimateThreshold ( code, confidence ) =
    if confidence < 0.5 then
        Nothing
    else
        Just code


{-| The function takes a gamepad state and returns a guess of the origin code
currently activated by the player.
-}
estimateOriginCode : Gamepad.RawGamepad -> Maybe String
estimateOriginCode gamepadState =
    gamepadState
        |> addRawGamepadToEstimates
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.head
        |> Maybe.andThen estimateThreshold


someButtonsArePressed pad =
    estimateThreshold pad /= Nothing


noButtonsArePressed pad =
    estimateThreshold pad == Nothing



{-

   TEA

-}


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



--


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


configuredEntriesToGamepadConfigString : List ConfiguredEntry -> String
configuredEntriesToGamepadConfigString entries =
    let
        entryToConfig entry =
            mappableControlToDestinationCode entry.destination ++ ":" ++ entry.originCode
    in
        entries
            |> List.map entryToConfig
            |> String.join ","


onButtonPress : String -> Model -> Outcome
onButtonPress originCodeOfPressedButton model =
    case model.unconfiguredEntries of
        [] ->
            Configured <| configuredEntriesToGamepadConfigString model.configuredEntries

        currentEntry :: remainingEntries ->
            let
                entryConfig =
                    { destination = currentEntry.destination
                    , originCode = originCodeOfPressedButton
                    }
            in
                StillOpen
                    { model
                        | configuredEntries = entryConfig :: model.configuredEntries
                        , unconfiguredEntries = remainingEntries
                    }


onGamepad : Time -> Gamepad.RawGamepad -> Model -> Outcome
onGamepad dt rawGamepad model =
    case ( model.inputState, estimateOriginCode rawGamepad ) of
        ( WaitingForAllButtonsUp, Just originCodeOfPressedButton ) ->
            StillOpen model

        ( WaitingForAllButtonsUp, Nothing ) ->
            StillOpen { model | inputState = WaitingForAnyButtonDown }

        ( WaitingForAnyButtonDown, Just originCodeOfPressedButton ) ->
            onButtonPress originCodeOfPressedButton { model | inputState = WaitingForAllButtonsUp }

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
                        noCmd <| onGamepad dt rawGamepad model



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
