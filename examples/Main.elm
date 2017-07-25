module Main exposing (..)

import Array
import Gamepad
import Gamepad.Remap
import GamepadPort
import Html exposing (..)
import Html.Attributes as HA
import Time exposing (Time)


type alias Model =
    { time : Time
    , blob : Maybe Gamepad.Blob
    , remap : Gamepad.Remap.Model
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )
    | OnRemapMsg Gamepad.Remap.Msg



--


noCmd model =
    ( model, Cmd.none )


init =
    let
        ( remapModel, remapCmd ) =
            Gamepad.Remap.initFullRemap 0
    in
        ( { time = 0
          , blob = Nothing
          , remap = remapModel
          }
        , Cmd.map OnRemapMsg remapCmd
        )


update msg model =
    case msg of
        OnGamepad ( time, blob ) ->
            noCmd { model | blob = Just blob }

        OnRemapMsg nestedMsg ->
            let
                ( outcome, cmd ) =
                    Gamepad.Remap.update nestedMsg model.remap

                newModel =
                    case outcome of
                        Gamepad.Remap.StillOpen nestedModel ->
                            { model | remap = nestedModel }

                        Gamepad.Remap.Done config ->
                            let
                                q =
                                    Debug.log "config ->" config
                            in
                                model
            in
                ( newModel, Cmd.map OnRemapMsg cmd )



--


viewGamepad : Gamepad.Connection -> Html msg
viewGamepad connection =
    case connection of
        Gamepad.Disconnected ->
            text "disconnected"

        Gamepad.Unrecognised ->
            text "not recognised"

        Gamepad.Available pad ->
            [ ( "a", Gamepad.aIsPressed >> toString )
            , ( "b", Gamepad.bIsPressed >> toString )
            , ( "x", Gamepad.xIsPressed >> toString )
            , ( "y", Gamepad.yIsPressed >> toString )

            --
            , ( "start", Gamepad.startIsPressed >> toString )
            , ( "back", Gamepad.backIsPressed >> toString )
            , ( "guide", Gamepad.guideIsPressed >> toString )

            --
            , ( "leftX", Gamepad.leftX >> toString )
            , ( "leftY", Gamepad.leftY >> toString )
            , ( "rightX", Gamepad.rightX >> toString )
            , ( "rightY", Gamepad.rightY >> toString )
            ]
                |> List.map (\( s, f ) -> div [] [ text s, text ": ", text <| f pad ])
                |> div []


showRaw raw =
    raw.buttons
        |> Array.toList
        |> List.indexedMap (\index b -> div [] [ text <| (toString index) ++ " " ++ (toString b) ])
        |> div []


view model =
    case model.blob of
        Nothing ->
            text ""

        Just blob ->
            div
                []
                [ div
                    [ HA.style
                        [ ( "display", "flex" )
                        , ( "justify-content", "center" )
                        , ( "width", "100%" )
                        ]
                    ]
                    [ Gamepad.Remap.view model.remap |> Html.map OnRemapMsg ]
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
        , Gamepad.Remap.subscriptions GamepadPort.gamepad model.remap |> Sub.map OnRemapMsg
        ]



-- main


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
