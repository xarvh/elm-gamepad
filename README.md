Elm Gamepad [![Travis build Status](https://travis-ci.org/xarvh/elm-gamepad.svg?branch=master)](http://travis-ci.org/xarvh/elm-gamepad)
===========


This library provides an interface to the [Navigator.getGamepads() Web API](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/getGamepads),
and the tools to remap gamepads and game controllers.

Since pure Elm cannot access `navigator.getGamepads()`, in order to use the
library you will need to manually add a port; you can use the one provided in
[port/](https://github.com/xarvh/elm-gamepad/tree/master/port).

* [See a running version of examples/Main.elm](https://xarvh.github.io/elm-gamepad/examples/)

* [See an actual game that uses the library](https://xarvh.github.io/elm-haifisch/)


```elm
import Gamepad exposing (Gamepad)
import GamepadPort
import Time exposing (Time)


type alias PlayerControl =
    { playerId : Int
    , isFiring : Bool
    , speed : Float
    }


type alias Model =
    { gamepadDatabase : Gamepad.Database
    , playerControls : List PlayerControl
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )


gamepadToPlayerControl : Gamepad -> PlayerControl
gamepadToPlayerControl gamepad =
    { playerId = Gamepad.getIndex gamepad
    , isFiring = Gamepad.rightTriggerIsPressed gamepad
    , speed = Gamepad.leftX gamepad
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnGamepad ( timeSinceLastFrameUpdate, blob ) ->
            let
                gamepads =
                    Gamepad.getGamepads model.gamepadDatabase blob

                playerControls =
                    List.map gamepadToPlayerControl gamepads
            in
                { model | playerControls = playerControls }


subscriptions : Model -> Sub Msg
subscriptions model =
    GamepadPort.gamepad OnGamepad

```



Important!
==========

Gamepad polling should be synchronised with the browser's animation frame.

If you are using [elm-lang/animation-frame](http://package.elm-lang.org/packages/elm-lang/animation-frame/latest)
you should remove it, and instead use the `Time` provided by the gamepad port,
which works like the value provided by [AnimationFrame.diffs](http://package.elm-lang.org/packages/elm-lang/animation-frame/latest/AnimationFrame#diffs).



Adding ports
============

The ports required by elm-gamepad are no different than any other [Elm port](https://guide.elm-lang.org/interop/javascript.html).

You can see how they are wired in in the [example's index.html](https://github.com/xarvh/elm-gamepad/blob/master/examples/index.html).

You can get ready-to-use port code from [port/](https://github.com/xarvh/elm-gamepad/tree/master/port); you will need to:

* Manually copy `GamepadPort.elm` in your Elm sources directory, so that you can import it as `GamepadPort`

* Import `gamepadPort.js` in your app JavaScript:
```html
<script type="text/javascript" src="gamepadPort.js"></script>
```

* Register the port with the Elm app:
```javascript
  var elmApp = Elm.Main.fullscreen();
  addGamepadPort(elmApp);
```

If you do not have another way to persist the gamepad database, you will want
to add also the local storage port, the procedure is exactly the same.



What's the problem with Browsers+Gamepads?
==========================================
The [w3c spec](https://www.w3.org/TR/gamepad/) defines a
["Standard Gamepad"](https://www.w3.org/TR/gamepad/#remapping) but the support
for this standard mapping is sparse and ultimately unreliable.

More in general, the same hardware will be recognised differently depending on
the operative system, installed driver, browser and even browser version.

For example under my Ubuntu, Firefox considers the trigger buttons of my Xbox360
gamepads as `axes`, while Chrome detects them as `buttons`.

Because of this, I don't think there is a way to create a reliable database of
all game controllers.

The only reliable option is to let the user remap their controller.
Remapping capabilities are also important to make a web application accessible.
