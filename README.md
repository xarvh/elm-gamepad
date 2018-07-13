Elm Gamepad [![Travis build Status](https://travis-ci.org/xarvh/elm-gamepad.svg?branch=master)](http://travis-ci.org/xarvh/elm-gamepad)
===========

![Standard Gamepad](https://xarvh.github.io/elm-gamepad/controller.svg)

This library allows you to use game controller aka gamepads in your Elm web app.

* [See a running version of examples/Main.elm](https://xarvh.github.io/elm-gamepad/examples/)

* [See an actual game that uses the library](https://xarvh.github.io/herzog-drei/)

**Important**: to avoid fingerprinting, the browser won't make gamepads visible until they are
touched by the user!

To use the library you need to **manually add a port**.
You can use the one provided in [port/](https://github.com/xarvh/elm-gamepad/tree/master/port).
See *Adding Ports* below.

Browser gamepad support is very inconsistent and varies wildly with the browser, the
browser version, the operative system and the installed gamepad drivers.

If you are lucky, the browser will recognize your gamepad(s) as a
[Standard Gamepad](https://www.w3.org/TR/gamepad/#remapping) which means that
you can use it with no remapping or configuration.

Often times however, the browser does not recognise the gamepad: in this case
you can still use it, but you will need to remap it with
[the remap tool provided](http://package.elm-lang.org/packages/xarvh/elm-gamepad/latest/Gamepad#RemapModel).
If you want everyone to be able to use your app, including gamepad remapping is
super important.




```elm
import Gamepad exposing (Gamepad)
import GamepadPort


type alias PlayerControl =
    { playerId : Int
    , isFiring : Bool
    , speed : Float
    }


type alias Model =
    { controls : List PlayerControl }


init : Model
init =
    { controls = [] }


type Msg
    = OnGamepad Gamepad.Blob


gamepadToPlayerControl : Gamepad -> PlayerControl
gamepadToPlayerControl gamepad =
    { playerId = Gamepad.getIndex gamepad
    , isFiring = Gamepad.isPressed gamepad Gamepad.A
    , speed = Gamepad.value gamepad Gamepad.LeftX
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnGamepad blob ->
            ( updateOnGamepad blob model, Cmd.none )


updateOnGamepad : Gamepad.Blob -> Model -> Model
updateOnGamepad blob model =
    let
        dt =
            -- Always cap, in case the page is hidden
            -- and refresh stops for a while
            min 200 (Gamepad.animationFrameDelta blob)

        gamepads =
            Gamepad.getGamepads
                Gamepad.emptyUserMappings
                blob

        controls =
            List.map gamepadToPlayerControl gamepads
    in
    { model | controls = controls }


subscriptions : Model -> Sub Msg
subscriptions model =
    GamepadPort.gamepad OnGamepad
```


Adding ports
============

The ports required by elm-gamepad are no different than any other [Elm port](https://guide.elm-lang.org/interop/javascript.html).

You can see how they are wired in in the [example's index.html](https://github.com/xarvh/elm-gamepad/blob/master/examples/index.html).

You can get ready-to-use port code from [port/](https://github.com/xarvh/elm-gamepad/tree/master/port):

* Manually copy `GamepadPort.elm` in your Elm sources directory, so that you can import it as `GamepadPort`

* Import `gamepadPort.js` in your app JavaScript:
```html
<script type="text/javascript" src="gamepadPort.js"></script>
```

* Register the port with the Elm app:
```javascript
  var elmApp = Elm.Main.init();
  addGamepadPort(elmApp);
```

If you do not have another way to persist the user mappings, you will want
to add also the local storage port, the procedure is exactly the same.
