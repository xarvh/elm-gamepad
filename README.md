Elm Gamepad
===========

![Standard Gamepad](https://xarvh.github.io/elm-gamepad/controller.svg)

This library allows you to use game controllers aka gamepads in your Elm web app.

* [See a running version of examples/simple](https://xarvh.github.io/elm-gamepad/examples/simple)

* [See a running version of examples/advanced](https://xarvh.github.io/elm-gamepad/examples/advanced)

* [See an actual game that uses the library](https://xarvh.github.io/herzog-drei/)

**Important**: to avoid fingerprinting, the browser won't make gamepads
visible until they are touched by the user!

To use this library you need to **manually add a port**.
You can use the one provided in [port/](https://github.com/xarvh/elm-gamepad/tree/master/port).
See *Adding Ports* below.

Browser gamepad support is very inconsistent and varies wildly with the
browser, the browser version, the operative system and the installed gamepad
drivers: this means that many gamepads can be reliably used only after
remapping.

If you have never used the library before, [Gamepad.Simple](Gamepad-Simple)
will manage the remapping tool for you.
If you need more control of how the remapping tool is summoned or how
the user-created settings are persisted, you will need to use
[Gamepad.Advanced](Gamepad-Advanced) instead.

The language of the text in the remapping tool will be translated according
to `navigator.languages`.


```elm
import Gamepad exposing (Gamepad)
import Gamepad.Simple
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
    = OnAnimationFrame Gamepad.Simple.FrameStuff


gamepadToPlayerControl : Gamepad -> PlayerControl
gamepadToPlayerControl gamepad =
    { playerId = Gamepad.getIndex gamepad
    , isFiring = Gamepad.isPressed gamepad Gamepad.A
    , speed = Gamepad.value gamepad Gamepad.LeftX
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame { gamepads, timestamp, dt } ->
            updateOnGamepad dt gamepads model


updateOnGamepad : Float -> List Gamepad -> Model -> Model
updateOnGamepad uncappedDt gamepads model =
    let
        dt =
            -- Always cap, in case the page is hidden
            -- and refresh stops for a while
            min 200 uncappedDt

        controls =
            List.map gamepadToPlayerControl gamepads
    in
    { model | controls = controls }


main =
    Gamepad.Simple.sandbox
        { onAnimationFrame = OnAnimationFrame
        , onBlob = GamepadPort.onBlob
        , saveToLocalStorage = GamepadPort.saveToLocalStorage
        , controls = Gamepad.Simple.basicControls
        }
        { init = init
        , view = view
        , update = update
        }
```


Known Issues
============

* The remapping tool will probably fail with gamepads that have inertial
sensors.
Distinguishing those from normal inputs while at the same time giving clear
and easy instructions to the user is tricky, and I won't be able to do much
until I get my hands on an actual gamepad with inertial sensors.

* Any solution to the problem above will probably require to change the
messages in the UI. Because of this, I will not commit to translations until
the remapping tool UI is in a more stable state; for the time being, only
English and French will be available.


Adding ports
============

The ports required by elm-gamepad are no different than any other [Elm port](https://guide.elm-lang.org/interop/ports.html).

You can see how they are wired in in the [example's index.html](https://github.com/xarvh/elm-gamepad/blob/master/examples/simple/index.html).

The ready-to-use port code is in [port/](https://github.com/xarvh/elm-gamepad/tree/master/port):

* Manually copy `GamepadPort.elm` in your Elm sources directory, so that you can import it as `GamepadPort`

* Manually copy `gamepadPort.js` so that it is available from to the browser

* Import `gamepadPort.js` in your app JavaScript:
```html
<script type="text/javascript" src="gamepadPort.js"></script>
```

* Register the port with the Elm app:
```javascript
  var elmApp = Elm.Main.init();
  addGamepadPort(elmApp);
```
