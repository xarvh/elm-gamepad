Elm Gamepad [![Travis build Status](https://travis-ci.org/xarvh/elm-gamepad.svg?branch=master)](http://travis-ci.org/xarvh/elm-gamepad)
===========

This library allows you to use game controller aka gamepads in your Elm web app.

* [See a running version of examples/Main.elm](https://xarvh.github.io/elm-gamepad/examples/)

* [See an actual game that uses the library](https://xarvh.github.io/elm-haifisch/)

Since pure Elm cannot access the [Navigator.getGamepads() Web API](https://developer.mozilla.org/en-US/docs/Web/API/Navigator/getGamepads)
that this library uses, **you will need to manually add a port**.
You can use the one provided in [port/](https://github.com/xarvh/elm-gamepad/tree/master/port).

Gamepad support is very inconsistent and varies wildly with the browser, the
browser version, the operative system and the installed gamepad drivers.

If you are lucky, the browser will recognize your gamepad(s) as a
["Standard Gamepad"](https://www.w3.org/TR/gamepad/#remapping) which means that
you can use it with no remapping or configuration.

Often times however, the browser does not recognise a gamepad: in this case you
can still use it, but you will need to remap it.
If you want everyone to be able to use your app, it should include a way to
remap the gamepads: the `Gamepad.Remap` module will help you with that.



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
