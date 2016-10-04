# Elm Native bindings to navigator.getGamepads()

[See a running version of examples/Main.elm](https://xarvh.github.io/elm-gamepad/).


This package has Native dependencies, and therefore can't be published as a normal
Elm package.
To use it, download it and then install it using `elm_self_publish` from
[NoRedInk/elm-ops-tooling](https://github.com/NoRedInk/elm-ops-tooling).


Just replace [`AnimationFrame.diffs`](http://package.elm-lang.org/packages/elm-lang/animation-frame/1.0.0/AnimationFrame#diffs)
with
```
animationFrameAndGamepads : (( Time, List Gamepad ) -> msg) -> Sub msg
```


This is what you get for each connected gamepad:
```
type alias Gamepad =
    { index : Int
    , axes : List Float
    , buttons : List ( Bool, Float )
    , connected : Bool
    }
```

Important: the [w3c spec](https://www.w3.org/TR/gamepad/) is still very fresh and different browsers
will map the same gamepad differently; for example, Firefox considers the shoulder buttons of my Xbox360
gamepads as axes, while Chrome detects them as Buttons. The two browsers even produce a different DOMString.

Next step could be a module that allows to configure the gamepads and save the setting in the browser local storage?
