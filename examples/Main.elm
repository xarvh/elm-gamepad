module Main exposing (..)

import Array
import Gamepad
import Gamepad.Remap exposing (MappableControl(..), Outcome(..))
import GamepadPort
import Html exposing (..)
import Html.Attributes as HA
import Time exposing (Time)


type alias Model =
    { time : Time
    , blob : Maybe Gamepad.Blob
    , remapOutcome : Gamepad.Remap.Outcome
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )
    | OnRemapMsg Gamepad.Remap.Msg



-- Mapping


inputNames =
    [ ( LeftX, "Move > Right" )
    , ( LeftY, "Move ^ Up" )
    , ( RightX, "Aim > Right" )
    , ( RightY, "Aim ^ Up" )
    , ( A, "Fire" )
    , ( B, "Fire (alternate button)" )
    ]


moveX =
    Gamepad.leftX


moveY =
    Gamepad.leftY


aimX =
    Gamepad.rightX


aimY =
    Gamepad.rightY


shootIsPressed pad =
    Gamepad.aIsPressed pad || Gamepad.bIsPressed pad



-- init


init =
    let
        ( remapModel, remapCmd ) =
            Gamepad.Remap.init 0 inputNames
    in
        ( { time = 0
          , blob = Nothing
          , remapOutcome = StillOpen remapModel
          }
        , Cmd.map OnRemapMsg remapCmd
        )



-- update


noCmd model =
    ( model, Cmd.none )


update msg model =
    case msg of
        OnGamepad ( time, blob ) ->
            noCmd { model | blob = Just blob }

        OnRemapMsg nestedMsg ->
            case model.remapOutcome of
                StillOpen nestedModel ->
                    let
                        ( outcome, cmd ) =
                            Gamepad.Remap.update nestedMsg nestedModel
                    in
                        ( { model | remapOutcome = outcome }, cmd |> Cmd.map OnRemapMsg )

                _ ->
                    noCmd model



-- view


viewGamepad : Gamepad.Connection -> Html msg
viewGamepad connection =
    case connection of
        Gamepad.Disconnected ->
            text "disconnected"

        Gamepad.Unrecognised ->
            text "not recognised"

        Gamepad.Available pad ->
            let
                ts ( name, getter ) =
                    toString name ++ ": " ++ toString (getter pad)
            in
                [ ts ( A, Gamepad.aIsPressed )
                , ts ( B, Gamepad.bIsPressed )
                , ts ( X, Gamepad.xIsPressed )
                , ts ( Y, Gamepad.yIsPressed )

                --
                , ts ( Start, Gamepad.startIsPressed )
                , ts ( Back, Gamepad.backIsPressed )
                , ts ( Guide, Gamepad.guideIsPressed )

                --
                , ts ( LeftX, Gamepad.leftX )
                , ts ( LeftY, Gamepad.leftY )
                , ts ( RightX, Gamepad.rightX )
                , ts ( RightY, Gamepad.rightY )

                --
                , ts ( DpadUp, Gamepad.dpadUp )
                , ts ( DpadDown, Gamepad.dpadDown )
                , ts ( DpadLeft, Gamepad.dpadLeft )
                , ts ( DpadRight, Gamepad.dpadRight )
                , ts ( "dpadX", Gamepad.dpadX )
                , ts ( "dpadY", Gamepad.dpadY )
                ]
                    |> List.map (\s -> div [] [ text s ])
                    |> div []


showRaw raw =
    raw.buttons
        |> Array.toList
        |> List.indexedMap (\index b -> div [] [ text <| (toString index) ++ " " ++ (toString b) ])
        |> div []


viewRemap model =
    div
        [ HA.style
            [ ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "width", "100%" )
            ]
        ]
        [ case model.remapOutcome of
            Configured string ->
                code
                    []
                    [ text string ]

            Aborted ->
                text "aborted"

            Disconnected ->
                text "disconnected"

            StillOpen nestedModel ->
                Gamepad.Remap.view nestedModel
        ]


view model =
    case model.blob of
        Nothing ->
            text ""

        Just blob ->
            div
                []
                [ viewRemap model |> Html.map OnRemapMsg
                , List.range 0 3
                    |> List.map (Gamepad.getGamepad blob)
                    |> List.map viewGamepad
                    |> List.map (\h -> li [] [ h ])
                    |> ul []
                , blob
                    |> List.filterMap identity
                    |> List.map showRaw
                    |> div []
                ]



-- subscriptions


subscriptions model =
    Sub.batch
        [ GamepadPort.gamepad OnGamepad
        , case model.remapOutcome of
            StillOpen nestedModel ->
                Gamepad.Remap.subscriptions GamepadPort.gamepad nestedModel |> Sub.map OnRemapMsg

            _ ->
                Sub.none
        ]



-- main


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
