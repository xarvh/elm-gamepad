effect module Gamepad where { subscription = MySub } exposing (Gamepad, animationFrameAndGamepads)

{-|

@docs Gamepad
@docs animationFrameAndGamepads

-}

import Json.Decode as D exposing ((:=))
import Native.Gamepad
import Process
import Task exposing (Task)
import Time exposing (Time)


{-| Describes a generic gamepad
-}
type alias Gamepad =
    { index : Int
    , axes : List Float
    , buttons : List ( Bool, Float )
    , id : String
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
        ("id" := D.string)


jsonToGamepads : D.Value -> List Gamepad
jsonToGamepads gamepadsAsJson =
    D.decodeValue (D.list decodeGamepad) gamepadsAsJson
        |> Result.withDefault []



-- SUBSCRIPTION


type alias TimeAndGamepads =
    ( Time, List Gamepad )


type MySub msg
    = AniFrame (TimeAndGamepads -> msg)


nativeTask : Task x { time : Time, gamepads : D.Value }
nativeTask =
    Native.Gamepad.animationFrameAndGamepads


animationFrameAndGamepadsTask : Task x TimeAndGamepads
animationFrameAndGamepadsTask =
    Task.map (\{ time, gamepads } -> ( time, jsonToGamepads gamepads )) nativeTask


subMap : (a -> b) -> MySub a -> MySub b
subMap f (AniFrame tagger) =
    AniFrame (f << tagger)


{-|
    Replaces [AnimationFrame.diffs](http://package.elm-lang.org/packages/elm-lang/animation-frame/latest).

    Requests the browser's animationFrame AND gamepad status at the same time.

    There are no events defined for gamepad signals so gamepads must be polled, and the best time when to
    do this is
    ["immediately before the animation callbacks are executed"](https://w3c.github.io/gamepad/#usage-examples).
-}
animationFrameAndGamepads : (TimeAndGamepads -> msg) -> Sub msg
animationFrameAndGamepads tagger =
    subscription (AniFrame tagger)


type alias State msg =
    { subs : List (MySub msg)
    , request : Maybe Process.Id
    , oldTime : Time
    }


init : Task Never (State msg)
init =
    Task.succeed (State [] Nothing 0)


onEffects : Platform.Router msg TimeAndGamepads -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router subs { request, oldTime } =
    case ( request, subs ) of
        ( Nothing, [] ) ->
            Task.succeed (State [] Nothing oldTime)

        ( Just pid, [] ) ->
            Process.kill pid `Task.andThen` \_ ->
            Task.succeed (State [] Nothing oldTime)

        ( Nothing, _ ) ->
            Process.spawn (animationFrameAndGamepadsTask `Task.andThen` Platform.sendToSelf router) `Task.andThen` \pid ->
            Time.now `Task.andThen` \time ->
            Task.succeed (State subs (Just pid) time)

        ( Just _, _ ) ->
            Task.succeed (State subs request oldTime)


onSelfMsg : Platform.Router msg TimeAndGamepads -> TimeAndGamepads -> State msg -> Task Never (State msg)
onSelfMsg router ( newTime, gamepads ) { subs, oldTime } =
    let
        diff =
            newTime - oldTime

        send (AniFrame tagger) =
            Platform.sendToApp router (tagger ( diff, gamepads ))
    in
        Process.spawn (animationFrameAndGamepadsTask `Task.andThen` Platform.sendToSelf router) `Task.andThen` \pid ->
        Task.sequence (List.map send subs) `Task.andThen` \_ ->
        Task.succeed (State subs (Just pid) newTime)
