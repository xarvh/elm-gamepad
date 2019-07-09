port module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode as Decode
import List.Extra
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Attribute msg =
    Html.Attribute msg


class =
    Html.Attributes.class


text =
    Html.text


node =
    Html.node


width =
    Svg.Attributes.width


height =
    Svg.Attributes.height



-- GAMEPAD API

port onBlob : (Blob -> msg) -> Sub msg


type alias Blob =
    ( BlobFrame, BlobFrame, Environment )


type alias BlobFrame =
    { gamepads : List GamepadFrame
    , timestamp : Float
    }


type alias GamepadFrame =
    { axes : Array Float
    , buttons : Array ( Bool, Float )
    , id : String
    , index : Int
    , mapping : String
    }


type alias Environment =
    { userMappings : String
    , languages : List String
    }


{-| TODO rename to InputId?
-}
type Digital
    = A
    | B
    | X
    | Y
    | Start
    | Back
    | Home
    | LeftStickPress
    | LeftStickUp
    | LeftStickDown
    | LeftStickLeft
    | LeftStickRight
    | LeftTrigger
    | LeftBumper
    | RightStickPress
    | RightStickUp
    | RightStickDown
    | RightStickLeft
    | RightStickRight
    | RightTrigger
    | RightBumper
    | DpadUp
    | DpadDown
    | DpadLeft
    | DpadRight



-- GAMEPAD REMAP API


type SignalId
    = Axis Int
    | Button Int


type InputState
    = Disabled
    | Unmapped
    | Mapped { signalId : SignalId, value : Float }


type GamepadMsg
    = OnClickTarget (Maybe Selection)
    | OnCloseMappingTool


type Selection
    = SelectedDigital Digital
    | SelectedSignal SignalId


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


s =
    Html.Attributes.style


userMustSelectADigital : ViewGamepadArgs -> Bool
userMustSelectADigital args =
    case args.maybeSelection of
        Just (SelectedSignal _) ->
            True

        _ ->
            False


userMustSelectASignal : ViewGamepadArgs -> Bool
userMustSelectASignal args =
    case args.maybeSelection of
        Just (SelectedDigital _) ->
            True

        _ ->
            False


viewDigital : ViewGamepadArgs -> (ViewGamepadArgs -> InputState) -> Digital -> String -> Html GamepadMsg
viewDigital args get digital content =
    let
        color =
            if args.maybeSelection == Just (SelectedDigital digital) then
                "orange"
            else
                case get args of
                    Disabled ->
                        "grey"

                    Unmapped ->
                        "red"

                    Mapped { signalId, value } ->
                        if userMustSelectADigital args then
                            "orange"
                        else
                            "green"

        signal =
            case get args of
                Mapped { signalId, value } ->
                    viewSignal args (Just signalId) value

                _ ->
                    viewSignal args Nothing 0
    in
    div
        [ digital
            |> SelectedDigital
            |> Just
            |> OnClickTarget
            |> onClick
        , s "border" "2px solid black"
        , s "border-color" color
        ]
        [ text content
        , signal
        ]


viewSignal : ViewGamepadArgs -> Maybe SignalId -> Float -> Html GamepadMsg
viewSignal args maybeId value =
    let
        opacity =
            0.3 + 0.7 * abs value

        color =
            if userMustSelectASignal args then
                "green"
            else
                "gray"
    in
    div
        [ maybeId
            |> Maybe.map (SelectedSignal >> Just >> OnClickTarget >> onClick)
            |> Maybe.withDefault (s "" "")
        , s "background-color" color
        , s "opacity" (String.fromFloat opacity)
        ]
        [ maybeId
            |> Maybe.map Debug.toString
            |> Maybe.withDefault ""
            |> text
        ]


userViewGamepad : ViewGamepadArgs -> Html GamepadMsg
userViewGamepad args =
    div
        [ class "gamepad"
        , onClick (OnClickTarget Nothing)
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
                |> List.map (\( signalId, value ) -> viewSignal args (Just signalId) value)
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


    """



-- App


type alias Model =
    { map : List ( Digital, SignalId )
    , values : List ( SignalId, Float )
    , maybeSelection : Maybe Selection
    }


init : Model
init =
    { map =
        []
    , values =
        [ ( Button 0, 0.8 )
        , ( Axis 0, -1 )
        ]
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
        OnAnimationFrame (old, new, env) ->
            model

        OnReset ->
            init

        OnGamepadMsg gamepadMsg ->
            case gamepadMsg of
                OnCloseMappingTool ->
                    model

                OnClickTarget target ->
                    case ( target, model.maybeSelection ) of
                        ( Just (SelectedDigital digital), Just (SelectedSignal signalId) ) ->
                            { model
                                | map = insert ( digital, signalId ) model.map
                                , maybeSelection = Nothing
                            }

                        ( Just (SelectedSignal signalId), Just (SelectedDigital digital) ) ->
                            { model
                                | map = insert ( digital, signalId ) model.map
                                , maybeSelection = Nothing
                            }

                        _ ->
                            { model | maybeSelection = target }


{-| One day Dicts will be able to use union types as keys
-}
insert : ( a, b ) -> List ( a, b ) -> List ( a, b )
insert ( digital, signalId ) list =
    list
        |> List.filter (\( d, id ) -> d /= digital)
        |> (::) ( digital, signalId )



--


type alias GaugeArgs =
    { maybeIndicator : Maybe ( Float, String )
    , color : String
    , text : String
    }


gauge : GaugeArgs -> Svg msg
gauge args =
    let
        thickness =
            "0.04"

        startAngle =
            -120

        endAngle =
            -startAngle

        ticks =
            9

        indicatorAngle value =
            startAngle + (endAngle - startAngle) * (0.5 + 0.5 * value)

        tickAngle index =
            startAngle + (endAngle - startAngle) * toFloat index / toFloat (ticks - 1)
    in
    svg
        [ viewBox "-1 -1 2 2"
        , width "60px"

        --, s "border" "1px solid black"
        ]
        [ List.range 0 (ticks - 1)
            |> List.map (tickAngle >> viewTick args.color thickness)
            |> g []
        , circle
            [ r "0.98"
            , fill "none"
            , stroke args.color
            , strokeWidth thickness
            ]
            []
        , case args.maybeIndicator of
            Just ( value, c ) ->
                value |> indicatorAngle |> viewIndicator c

            Nothing ->
                text ""
        , text_
            [ args.text
                |> String.length
                |> toFloat
                |> (*) 0.13
                |> String.fromFloat
                |> textLength
            , y "0.08"
            , lengthAdjust "spacingAndGlyphs"
            , fontSize "0.3"
            , textAnchor "middle"
            , fill args.color
            , s "font-family" "Sans-Serif"
            ]
            [ text args.text ]
        ]


viewIndicator : String -> Float -> Svg msg
viewIndicator color angle =
    rect
        [ transform <| "rotate(" ++ String.fromFloat angle ++ ") translate(0, -0.7)"
        , x "-0.02"
        , width "0.04"
        , y "-0.15"
        , height "0.3"
        , fill color
        ]
        []


viewTick : String -> String -> Float -> Svg msg
viewTick color thickness angle =
    rect
        [ transform <| "rotate(" ++ String.fromFloat angle ++ ") translate(0, -0.8)"
        , x "-0.01"
        , width thickness
        , y "-0.1"
        , height "0.2"
        , fill color
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


viewGauges : List (Html msg)
viewGauges =
    [ div
        []
        -- Unmapped Signal when nothing is selected: normal
        [ gauge { maybeIndicator = Just ( -1, "red" ), text = "Axis 1", color = "black" }

        -- Unmapped Signal when another Signal is selected: dimmed
        -- Mapped Signal when another Signal is selected: also dimmed?
        , gauge { maybeIndicator = Just ( 0, "gray" ), text = "Button 10", color = "lightgray" }

        -- Mapped Signal : text replaced with green checkmark?
        , gauge { maybeIndicator = Just ( 1, "green" ), text = "✔", color = "green" }

        -- Unmapped Digital: dimmed, no indicator, bounces if a Signal is selected, red question mark?
        , gauge { maybeIndicator = Nothing, text = "?", color = "red" }
        ]
    ]



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
        , view = \model -> { title = "meh", body = viewGauges }
        , subscriptions = \model -> onBlob OnAnimationFrame
        }
