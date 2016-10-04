module Gamepad exposing (Gamepad, gamepadAndAnimationFrame)

{-|
@docs Gamepad

@docs gamepadAndAnimationFrame
-}

import Json.Decode as D exposing ((:=))
import Native.Gamepad
import Task
import Time exposing (Time)


{-| Describes a generic gamepad
-}
type alias Gamepad =
    { index : Int
    , axes : List Float
    , buttons : List ( Bool, Float )
    , connected : Bool
    }


decodeButton =
    D.tuple2
        (,)
        D.bool
        D.float


decodeGamepad =
    D.object4
        Gamepad
        ("index" := D.int)
        ("axes" := D.list D.float)
        ("buttons" := D.list decodeButton)
        ("connected" := D.bool)


jsonToGamepads : D.Value -> List Gamepad
jsonToGamepads gamepadsAsJson =
    D.decodeValue (D.list decodeGamepad) gamepadsAsJson
        |> Result.withDefault []


{-|
    Requests the browser's animationFrame AND gamepad status at the same time.

    There are no events defined for gamepad signals so gamepads must be polled, and the best time when to
    do this is
    ["immediately before the animation callbacks are executed"](https://w3c.github.io/gamepad/#usage-examples).

    IMPORTANT
    ---------

    1. This module *replaces* [elm-lang/animation-frame](http://package.elm-lang.org/packages/elm-lang/animation-frame/latest)
    Don't use the two modules together

    2. This module provides a `Cmd`, not a subscription,
    mostly because subscriptions are a huge PITA to implement without docs.

    Every time the associated message triggers, YOU MUST EXECUTE IT AGAIN.
-}
gamepadAndAnimationFrame : (( Time, List Gamepad ) -> msg) -> Cmd msg
gamepadAndAnimationFrame tagger =
    Native.Gamepad.gamepadAndAnimationFrame
        |> Task.map (\{ dt, gamepads } -> ( dt, jsonToGamepads gamepads ))
        |> Task.perform identity tagger
