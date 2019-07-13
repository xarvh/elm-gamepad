module Remap exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Gamepad exposing (Digital, digitalToString)
import Gamepad.Private as Private exposing (Blob, BlobFrame, Gamepad, GamepadFrame, Origin)
import Html exposing (..)
import List.Extra


-- GAMEPAD REMAP API


type SignalId
    = Axis Int
    | Button Int


type InputState
    = Disabled
    | Unmapped
    | Mapped SignalId


type GamepadMsg
    = OnClickSignal SignalId
    | OnClickDigital { isInverted : Bool } Digital
    | OnCloseMappingTool


type Selection
    = SelectedSignal SignalId


type alias ViewGamepadArgs =
    { gamepad : Gamepad
    , unmappedSignals : List ( SignalId, Float )
    , maybeSelectedSignal : Maybe SignalId
    }


{-| TODO make this opaque
-}
type alias Model =
    { gamepadIndex : Int
    , targets : List Digital
    , blob : Blob
    , mapping : Private.Mapping
    , maybeSelectedSignal : Maybe SignalId
    }


init : Int -> List Digital -> Model
init index targets =
    { gamepadIndex = index
    , targets = targets
    , blob = Private.emptyBlob
    , mapping = Dict.empty
    , maybeSelectedSignal = Nothing
    }


reinit : Model -> Model
reinit model =
    { model
        | mapping = Dict.empty
        , maybeSelectedSignal = Nothing
    }


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



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame blob ->
            { model | blob = blob }

        OnReset ->
            reinit model

        OnGamepadMsg gamepadMsg ->
            case gamepadMsg of
                OnCloseMappingTool ->
                    model

                OnClickSignal signalId ->
                    { model | maybeSelectedSignal = Just signalId }

                OnClickDigital { isInverted } digital ->
                    case model.maybeSelectedSignal of
                        Nothing ->
                            model

                        Just signalId ->
                            { model
                                | maybeSelectedSignal = Nothing
                                , mapping = Dict.insert (digitalToString digital) (signalIdToOrigin isInverted signalId) model.mapping
                            }



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
