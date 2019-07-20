port module Main exposing (..)

import Browser
import CustomView
import Gamepad exposing (Digital(..))
import Gamepad.Advanced exposing (Blob)
import Html
import Remap


port onBlob : (Blob -> msg) -> Sub msg


targets : List Digital
targets =
    [ LeftStickUp
    , LeftStickDown
    , LeftStickLeft
    , LeftStickRight
    , A
    , B
    ]


main : Program {} Remap.Model Remap.Msg
main =
    Browser.document
        { init =
            \flags ->
                ( Remap.init 1 targets
                , Cmd.none
                )
        , update =
            \msg model ->
                ( Remap.update msg model
                , Cmd.none
                )
        , view =
            \model ->
                { title = "WIP"
                , body =
                    [ model
                        |> Remap.modelToRemappingState
                        |> CustomView.userViewGamepad
                        |> Html.map Remap.OnGamepadMsg
                    ]
                }
        , subscriptions =
            \model ->
                onBlob Remap.OnAnimationFrame
        }
