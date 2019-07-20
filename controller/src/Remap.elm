module Remap exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Gamepad exposing (Digital, digitalToString)
import Gamepad.Private as Private exposing (Blob, BlobFrame, Gamepad, GamepadFrame, Origin)
import Html exposing (..)
import List.Extra


-- API


type RemappingState
    = StateConnectionLost
    | StateManual
        { maybeSelectedSignal : Maybe SignalId
        , unmappedSignals : List ( SignalId, Float )
        , gamepad : Gamepad
        }
    | StateAutomaticOngoing
        { awaitingSignalForDigital : Digital
        }
    | StateAutomaticFinished


type GamepadMsg
    = OnClickSignal SignalId
    | OnClickDigital Digital
    | OnCloseMappingTool


type InputState
    = Disabled
    | Unmapped
    | Mapped SignalId



-- Messages


type Msg
    = OnGamepadMsg GamepadMsg
    | OnAnimationFrame Blob
    | OnReset



-- Model


type SignalId
    = AxisId Int
    | ButtonId Int


type SignalValue
    = AxisValue Float
    | ButtonValue Bool


type AcceptanceStatus
    = WasNeutral
    | WasNeutralThenActive


{-| TODO make this opaque
-}
type alias Model =
    { gamepadIndex : Int
    , targets : List Digital
    , blob : Blob
    , mapping : Private.Mapping
    , isAuto : Bool

    -- Auto mode
    , acceptanceStatusBySignalId : Dict String AcceptanceStatus

    -- Manual mode
    , maybeSelectedSignal : Maybe SignalId
    }


init : Int -> List Digital -> Model
init index targets =
    { gamepadIndex = index
    , targets = targets
    , blob = Private.emptyBlob
    , mapping = Dict.empty
    , isAuto = True

    --
    , acceptanceStatusBySignalId = Dict.empty

    --
    , maybeSelectedSignal = Nothing
    }



-- API


getInputState : Model -> Gamepad -> Digital -> InputState
getInputState model (Private.Gamepad mapping f1 f2) digital =
    case Dict.get (digitalToString digital) mapping of
        Nothing ->
            if List.any (\t -> t == digital) model.targets then
                Unmapped
            else
                Disabled

        Just origin ->
            Mapped (originToSignalId origin)



-- Mapping


originToSignalId : Origin -> SignalId
originToSignalId (Private.Origin isReversed type_ index) =
    case type_ of
        Private.Axis ->
            AxisId index

        Private.Button ->
            ButtonId index


signalIdToOrigin : Bool -> SignalId -> Origin
signalIdToOrigin isReverse signalId =
    case signalId of
        AxisId index ->
            Private.Origin isReverse Private.Axis index

        ButtonId index ->
            Private.Origin isReverse Private.Button index


getFirstUnmappedDigital : Model -> Maybe Digital
getFirstUnmappedDigital model =
    let
        isUnmapped digital =
            Dict.get (digitalToString digital) model.mapping == Nothing
    in
    List.Extra.find isUnmapped model.targets


insertPairInMapping : Digital -> SignalId -> Bool -> Model -> Model
insertPairInMapping digital signalId isReverse model =
    let
        ( currentFrame, _, _ ) =
            model.blob

        q =
            Debug.log "adding" ( digital, signalId, isReverse )
    in
    { model | mapping = Dict.insert (digitalToString digital) (signalIdToOrigin isReverse signalId) model.mapping }



-- Signal ids


signalIdToString : SignalId -> String
signalIdToString signalId =
    case signalId of
        AxisId index ->
            "a" ++ String.fromInt index

        ButtonId index ->
            "b" ++ String.fromInt index



-- Signal values


signalValueIsProbablyActive : SignalValue -> Bool
signalValueIsProbablyActive signalValue =
    case signalValue of
        AxisValue value ->
            abs value > 0.6

        ButtonValue state ->
            state


signalValueIsProbablyNeutral : SignalValue -> Bool
signalValueIsProbablyNeutral signalValue =
    case signalValue of
        AxisValue value ->
            abs value < 0.2

        ButtonValue state ->
            not state


signalValueIsReversed : SignalValue -> Bool
signalValueIsReversed signalValue =
    case signalValue of
        AxisValue value ->
            value < 0

        ButtonValue state ->
            False



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame blob ->
            { model | blob = blob }
                |> (if model.isAuto then
                        updateAuto
                    else
                        updateManual
                   )

        OnReset ->
            init model.gamepadIndex model.targets

        OnGamepadMsg gamepadMsg ->
            case gamepadMsg of
                OnCloseMappingTool ->
                    model

                OnClickSignal signalId ->
                    { model | maybeSelectedSignal = Just signalId }

                OnClickDigital digital ->
                    case model.maybeSelectedSignal of
                        Nothing ->
                            model

                        Just signalId ->
                            { model | maybeSelectedSignal = Nothing }
                                |> insertPairInMapping digital signalId (Debug.todo "isReverse?")



-- Manual mode


updateManual : Model -> Model
updateManual model =
    model



-- Automatic mode


updateAuto : Model -> Model
updateAuto model =
    case getFirstUnmappedDigital model of
        Nothing ->
            model

        Just digital ->
            model.blob
                |> blobToSignals model.gamepadIndex
                |> List.foldl (updateAcceptanceStatus digital) model


blobToSignals : Int -> Blob -> List ( SignalId, SignalValue )
blobToSignals gamepadIndex ( currentBlobFrame, _, _ ) =
    case List.Extra.find (\gamepad -> gamepad.index == gamepadIndex) currentBlobFrame.gamepads of
        Nothing ->
            []

        Just frame ->
            [ Array.indexedMap (\index value -> ( AxisId index, AxisValue value )) frame.axes
            , Array.indexedMap (\index ( state, value ) -> ( ButtonId index, ButtonValue state )) frame.buttons
            ]
                |> List.map Array.toList
                |> List.concat


updateAcceptanceStatus : Digital -> ( SignalId, SignalValue ) -> Model -> Model
updateAcceptanceStatus targetDigital ( signalId, signalValue ) model =
    let
        key =
            signalIdToString signalId
    in
    case Dict.get key model.acceptanceStatusBySignalId of
        Nothing ->
            -- Step 1: signal is initially neutral
            if signalValueIsProbablyNeutral signalValue then
                { model | acceptanceStatusBySignalId = Dict.insert key WasNeutral model.acceptanceStatusBySignalId }
            else
                model

        Just WasNeutral ->
            -- Step 2: signal is activated by the user
            if signalValueIsProbablyActive signalValue then
                { model | acceptanceStatusBySignalId = Dict.insert key WasNeutralThenActive model.acceptanceStatusBySignalId }
            else
                model

        Just WasNeutralThenActive ->
            -- Step 3: user releases the control again
            if signalValueIsProbablyNeutral signalValue then
                { model | acceptanceStatusBySignalId = Dict.empty }
                    |> insertPairInMapping targetDigital signalId (signalValueIsReversed signalValue)
            else
                model



-- View


modelToRemappingState : Model -> RemappingState
modelToRemappingState model =
    let
        ( currentBlobFrame, previousBlobFrame, env ) =
            model.blob

        getGamepadFrame blobFrame =
            blobFrame.gamepads
                |> List.Extra.find (\gamepadFrame -> gamepadFrame.index == model.gamepadIndex)
    in
    case Maybe.map2 Tuple.pair (getGamepadFrame currentBlobFrame) (getGamepadFrame previousBlobFrame) of
        Nothing ->
            StateConnectionLost

        Just ( currentGamepadFrame, previousGamepadFrame ) ->
            if model.isAuto then
                case getFirstUnmappedDigital model of
                    Nothing ->
                        StateAutomaticFinished

                    Just digital ->
                        StateAutomaticOngoing
                            { awaitingSignalForDigital = digital
                            }
            else
                StateConnectionLost



{-
               StateManual
                   { }

         currentGamepadFrame =
             getGamepadFrame currentBlobFrame

         previousGamepadFrame =
             getGamepadFrame previousBlobFrame

         mappedSignals =
             model.mapping
                 |> Dict.values
                 |> List.map originToSignalId

         unmappedSignals =
             [ currentGamepadFrame.buttons
                 |> Array.toList
                 |> List.indexedMap (\index button -> ( Button index, Tuple.second button ))
             , currentGamepadFrame.axes
                 |> Array.toList
                 |> List.indexedMap (\index value -> ( Axis index, value ))
             ]
                 |> List.concat
                 |> List.filter (\( s, v ) -> List.all ((/=) s) mappedSignals)

   in

     { gamepad = Private.Gamepad model.mapping currentGamepadFrame previousGamepadFrame
     , unmappedSignals = unmappedSignals
     , maybeSelectedSignal = model.maybeSelectedSignal
     }
-}
