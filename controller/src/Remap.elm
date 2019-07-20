module Remap exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Gamepad exposing (Digital, digitalToString)
import Gamepad.Private as Private exposing (Blob, BlobFrame, Gamepad, GamepadFrame, Origin)
import Html exposing (..)
import List.Extra


type alias Timestamp =
    Float



-- GAMEPAD REMAP API


type alias ViewGamepadArgs =
    { gamepad : Gamepad
    , unmappedSignals : List ( SignalId, Float )
    , maybeSelectedSignal : Maybe SignalId
    }


type GamepadMsg
    = OnClickSignal SignalId
    | OnClickDigital Digital
    | OnCloseMappingTool


type InputState
    = Disabled
    | Unmapped
    | Mapped SignalId



-- Model


type SignalId
    = AxisId Int
    | ButtonId Int


type SignalValue
  = AxisValue Float
  | ButtonValue Bool




type Mode
    = Auto
    | Manual


{-| TODO make this opaque
-}
type alias Model =
    { gamepadIndex : Int
    , targets : List Digital
    , blob : Blob
    , mapping : Private.Mapping
    , mode : Mode

    -- Auto mode
    , lastInactiveAt : Dict Signal Timestamp
    , lastMappingInsertAt : Timestamp

    -- Manual mode
    , maybeSelectedSignal : Maybe SignalId
    }


init : Int -> List Digital -> Model
init index targets =
    { gamepadIndex = index
    , targets = targets
    , blob = Private.emptyBlob
    , mapping = Dict.empty
    , mode = Auto

    --
    , lastInactiveAt = Dict.empty
    , lastMappingInsertAt = 0

    --
    , maybeSelectedSignal = Nothing
    }


reinit : Model -> Model
reinit model =
    init model.index model.targets


type Msg
    = OnGamepadMsg GamepadMsg
    | OnAnimationFrame Blob
    | OnReset



-- Exposed helpers


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



-- Non exposed helpers


originToSignalId : Origin -> SignalId
originToSignalId (Private.Origin isReversed type_ index) =
    case type_ of
        Private.Axis ->
            Axis index

        Private.Button ->
            Button index


signalIdToOrigin : Bool -> SignalId -> Origin
signalIdToOrigin isReverse signalId =
    case signalId of
        Axis index ->
            Private.Origin isReverse Private.Axis index

        Button index ->
            Private.Origin isReverse Private.Button index


getFirstUnmappedDigital : Model -> Maybe Digital
getFirstUnmappedDigital model =
    let
        isUnmapped digital =
            Dict.get (digitalToString digital) model.mapping == Nothing
    in
    List.Extra.find isUnmapped model.targets


insertPairInMapping : Digital -> SignalId -> Model -> Model
insertPairInMapping digital signalId model =
    { model | mapping = Dict.insert (digitalToString digital) (signalIdToOrigin False signalId) model.mapping }


-- Estimates




signalEstimateState : SignalEstimate -> SignalEstimateState
signalEstimateState estimate =





-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame blob ->
            -- TODO ===========================================================
            { model | blob = blob }

        OnReset ->
            reinit model

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
                                |> insertPairInMapping digital signalId



{-
   * update lastInactiveAt
   * add mapping
-}


stuff model blob =
    let
        ( currentBlobFrame, _, _ ) =
            blob

        now =
            currentBlobFrame.timestamp
    in
    case List.Extra.find (\pad -> pad.index == model.gamepadIndex) currentBlobFrame.gamepads of
        Nothing ->
            model

        Just currentGamepadFrame ->
            --           for any (signal, value) current
            case estimate value of
                Inactive ->
                    { model | lastInactiveAt = Dict.insert ( signal, now ) model.lastInactiveAt }

                Uncertain ->
                    model

                Active ->
                    case Dict.get signal model.lastInactiveAt of
                        Nothing ->
                            -- It has never been inactive so far
                            model

                        Just lastInactiveAt ->
                            if lastInactiveAt <= model.lastMappingInsertAt then
                                -- it didn't change since we last mapped something, so it's probably not the signal that the user wants
                                model
                            else
                                -- The user changed it, so it must be what they want mapped to the current digital
                                case getFirstUnmappedDigital model of
                                    Nothing ->
                                        model

                                    Just digital ->
                                        insertPairInMapping digital signalId model


updateLastInactiveTime : Timestamp -> List ( SignalId, SignalEstimate ) -> Model -> Model
updateLastInactiveTime now idsAndEstimates model =
    let
        maybeUpdateTime ( signalId, signalEstimate ) lastInactiveAt =
            if signalEstimateState signalEstimate /= Inactive then
                lastInactiveAt
            else
                Dict.insert (signalIdToString signalId) now lastInactiveAt
    in
    { model | lastInactiveAt = List.foldl maybeUpdateTime model.lastInactiveAt idsAndEstimates }


userWantsToUseThisSignal : Model -> ( SignalId, SignalEstimate ) -> Bool
userWantsToUseThisSignal model ( signalId, signalEstimate ) =
    if signalEstimateState signalEstimate /= Active then
        False
    else
        case Dict.get signal model.lastInactiveAt of
            Nothing ->
                -- It has never been inactive so far
                False

            Just lastInactiveAt ->
                -- Has it been released since last time we added
                lastInactiveAt > model.lastMappingInsertAt



-- View


modelToViewGamepadArgs : Model -> ViewGamepadArgs
modelToViewGamepadArgs model =
    let
        ( currentBlobFrame, previousBlobFrame, env ) =
            model.blob

        emptyGamepadFrame =
            { axes = Array.empty
            , buttons = Array.empty
            , id = ""
            , index = model.gamepadIndex
            , mapping = ""
            }

        getGamepadFrame blobFrame =
            blobFrame.gamepads
                |> List.Extra.find (\gamepadFrame -> gamepadFrame.index == model.gamepadIndex)
                |> Maybe.withDefault emptyGamepadFrame

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


view : (ViewGamepadArgs -> Html GamepadMsg) -> Model -> List (Html Msg)
view userViewGamepad model =
    let
        args =
            modelToViewGamepadArgs model
    in
    [ args
        |> userViewGamepad
        |> Html.map OnGamepadMsg
    , args
        |> Debug.toString
        |> Html.text
    ]
