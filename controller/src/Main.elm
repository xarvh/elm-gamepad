port module Main exposing (..)

import Array exposing (Array)
import Browser
import Gamepad exposing (Digital(..))
import Gamepad.Private exposing (Blob, BlobFrame, GamepadFrame)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Decode
import List.Extra
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)


type alias Attribute msg =
    Html.Attribute msg


class =
    Html.Attributes.class


text =
    Html.text


node =
    Html.node


port onBlob : (Blob -> msg) -> Sub msg



-- GAMEPAD REMAP API


type SignalId
    = Axis Int
    | Button Int


type InputState
    = Disabled
    | Unmapped
    | Mapped { signalId : SignalId, value : Float }


type GamepadMsg
    = OnClickSignal SignalId
    | OnClickDigital { isInverted : Bool } Digital
    | OnCloseMappingTool


type Selection
    = SelectedSignal SignalId


type alias ViewGamepadArgs =
    { dpadUp : InputState
    , dpadDown : InputState
    , a : InputState
    , b : InputState

    -- TODO: add all other inputs
    , unmappedSignals : List ( SignalId, Float )
    , maybeSelection : Maybe Selection
    }



--


onClick : msg -> Attribute msg
onClick msg =
    Html.Events.stopPropagationOn "click" (Decode.succeed ( msg, True ))


st =
    Html.Attributes.style


viewDigital : ViewGamepadArgs -> (ViewGamepadArgs -> InputState) -> Digital -> String -> Html GamepadMsg
viewDigital args get digital content =
    let
        color =
            case get args of
                Disabled ->
                    "grey"

                Unmapped ->
                    "red"

                Mapped { signalId, value } ->
                    "green"

        signal =
            case get args of
                Mapped { signalId, value } ->
                    viewSignal args signalId value

                _ ->
                    div
                        [ if args.maybeSelection /= Nothing then
                            class "call-attention"
                          else
                            class ""
                        ]
                        [ gauge
                            { maybeIndicator = Nothing
                            , color = "lightgray"
                            , center = gaugeQuestionMark "lightgray"
                            }
                        ]
    in
    div
        [ st "border" "2px solid black"
        , st "border-color" color
        , onClick (OnClickDigital { isInverted = False } digital)
        ]
        [ text content
        , signal
        ]


userViewGamepad : ViewGamepadArgs -> Html GamepadMsg
userViewGamepad args =
    div
        [ class "gamepad"

        --, onClick OnClickBackground
        ]
        [ div
            [ class "dpad"
            ]
            [ viewDigital args .dpadUp DpadUp "Up"
            , viewDigital args .dpadDown DpadDown "Down"
            ]
        , div
            [ class "unmapped"
            ]
            [ args.unmappedSignals
                |> List.map (\( signalId, value ) -> viewSignal args signalId value)
                |> div []
            ]
        ]


css : String
css =
    """


.gamepad {
  position: relative;

  width: 600px;
  height: 300px;
  border: 2px black solid;
}


.dpad {
  position: absolute;
  top: 50px;
  left: 50px;
  height: 200px;

  border: 2px black solid;

  display: flex;
  flex-direction: column;
  justify-content: space-between;
  padding: 5px;
}


.unmapped {
  position: absolute;
  top: 50px;
  left: 400px;
}




.call-attention {
  animation-name: callAttention;
  animation-duration: 0.3s;
  animation-timing-function: ease;
  animation-iteration-count: infinite;
  animation-direction: alternate;
}

@keyframes callAttention {
  from {
    transform: scale(1);
 }

  to {
    transform: scale(0.7);
 }
}



    """



-- App


type alias Model =
    -- TODO: map must support inverted digitals
    { map : List ( Digital, SignalId )
    , values : List ( SignalId, Float )
    , maybeSelection : Maybe Selection
    }


init : Model
init =
    { map = []
    , values = []
    , maybeSelection = Nothing
    }


type Msg
    = OnGamepadMsg GamepadMsg
    | OnAnimationFrame Blob
    | OnReset



-- App Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame ( old, new, env ) ->
            case new.gamepads of
                [] ->
                    model

                gamepad :: _ ->
                    { model
                        | values =
                            List.concat
                                [ gamepad.buttons
                                    |> Array.toList
                                    |> List.indexedMap (\index button -> ( Button index, Tuple.second button ))
                                , gamepad.axes
                                    |> Array.toList
                                    |> List.indexedMap (\index value -> ( Axis index, value ))
                                ]
                    }

        OnReset ->
            init

        OnGamepadMsg gamepadMsg ->
            case gamepadMsg of
                OnCloseMappingTool ->
                    model

                OnClickSignal signalId ->
                    { model | maybeSelection = Just (SelectedSignal signalId) }

                OnClickDigital { isInverted } digital ->
                    case model.maybeSelection of
                        Nothing ->
                            model

                        Just (SelectedSignal signalId) ->
                            { model
                                | maybeSelection =
                                    Nothing
                                , map =
                                    model.map
                                        |> List.filter (\( d, s ) -> d /= digital && s /= signalId)
                                        |> (::) ( digital, signalId )
                            }


viewSignal : ViewGamepadArgs -> SignalId -> Float -> Html GamepadMsg
viewSignal args id value =
    let
        color =
            if args.maybeSelection == Nothing || args.maybeSelection == Just (SelectedSignal id) then
                "gray"
            else
                "lightgray"
    in
    div
        [ onClick (OnClickSignal id) ]
        [ gauge
            { maybeIndicator = Just ( value, color )
            , color = color
            , center = gaugeText color (Debug.toString id)
            }
        ]



--


type alias GaugeArgs msg =
    { maybeIndicator : Maybe ( Float, String )
    , color : String
    , center : Svg msg
    }


gauge : GaugeArgs msg -> Svg msg
gauge args =
    let
        thickness =
            0.1

        startAngle =
            -120

        endAngle =
            -startAngle

        ticks =
            7

        indicatorAngle value =
            startAngle + (endAngle - startAngle) * (0.5 + 0.5 * value)

        tickAngle index =
            startAngle + (endAngle - startAngle) * toFloat index / toFloat (ticks - 1)
    in
    svg
        [ viewBox "-1 -1 2 2"
        , SA.width "60px"
        ]
        [ List.range 0 (ticks - 1)
            |> List.map (tickAngle >> viewTick args.color thickness)
            |> g []
        , circle
            [ r "0.9"
            , fill "none"
            , stroke args.color
            , strokeWidth <| String.fromFloat thickness
            ]
            []
        , case args.maybeIndicator of
            Just ( value, c ) ->
                value |> indicatorAngle |> viewIndicator c

            Nothing ->
                text ""
        , args.center
        ]


gaugeText : String -> String -> Svg msg
gaugeText color content =
    text_
        [ content
            |> String.length
            |> toFloat
            |> (*) 0.13
            |> String.fromFloat
            |> textLength
        , y "0.08"
        , lengthAdjust "spacingAndGlyphs"
        , fontSize "0.3"
        , textAnchor "middle"
        , fill color
        , st "font-family" "Sans-Serif"
        ]
        [ text content ]


gaugeQuestionMark : String -> Svg msg
gaugeQuestionMark color =
    text_
        [ SA.y "0.5"
        , SA.fontSize "1.5"
        , SA.textAnchor "middle"
        , SA.fill color
        , st "font-family" "Sans-Serif"
        ]
        [ text "?" ]


viewIndicator : String -> Float -> Svg msg
viewIndicator color angle =
    let
        width =
            0.1

        height =
            0.2
    in
    polygon
        [ SA.transform <| "rotate(" ++ String.fromFloat angle ++ ")"
        , SA.points "-0.2 -0.3, 0 -0.6, 0.2 -0.3"
        , SA.fill color
        ]
        []


viewTick : String -> Float -> Float -> Svg msg
viewTick color thickness angle =
    let
        height =
            0.2
    in
    rect
        [ SA.transform <| "rotate(" ++ String.fromFloat angle ++ ") translate(0, -0.7)"
        , SA.x <| String.fromFloat <| -thickness / 2
        , SA.width <| String.fromFloat thickness
        , SA.y <| String.fromFloat <| -height / 2
        , SA.height <| String.fromFloat height
        , SA.fill color
        ]
        []



{-
   YOU CANNOT SELECT DIGITALS!!!


   * Mapped Digital: green?



   - color schema
     normal
     dimmed
     green


-}
-- App View


view : Model -> List (Html Msg)
view model =
    let
        getValue signalId =
            model.values
                |> List.Extra.find (\( sid, v ) -> sid == signalId)
                |> Maybe.map Tuple.second
                -- Even if we lost the signal for some reason, it is still bound to the input, so we should display it
                |> Maybe.withDefault 0

        set : Digital -> InputState
        set digital =
            case List.Extra.find (\( d, signalId ) -> d == digital) model.map of
                Nothing ->
                    Unmapped

                Just ( d, signalId ) ->
                    Mapped
                        { signalId = signalId
                        , value = getValue signalId
                        }

        args =
            { dpadUp = set DpadUp
            , dpadDown = set DpadDown
            , a = set A
            , b = set B
            , unmappedSignals = model.values |> List.filter (\( id, value ) -> List.all (\( d, sig ) -> sig /= id) model.map)
            , maybeSelection = model.maybeSelection
            }
    in
    [ args
        |> userViewGamepad
        |> Html.map OnGamepadMsg
    , args
        |> Debug.toString
        |> text
    , node "style"
        []
        [ text css ]
    ]



-- Main


main : Program {} Model Msg
main =
    Browser.document
        { init = \flags -> ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = \model -> { title = "WIP", body = view model }
        , subscriptions = \model -> onBlob OnAnimationFrame
        }
