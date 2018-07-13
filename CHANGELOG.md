2.0.0
======
  * Switched to Elm 0.19. Elm 0.18 is not supported any more.

  * Button/Sticks API changed entirely, now it's possible to detect when
    a button has been clicked or released.

  * The funcitonality of `Gamepad.Remap` has been merged in `Gamepad` and
    standardised to Elm Architecture functions and types.
    Remap customisation beyond CSS restyling is not available any more.

  * Removed `Destination` type, `Digital` should be used instead.

  * `Database` renamed to `UserMappings`, the custom serialisation format
    has been replaced with JSON. Encoder and Decoder are also available.

  * It is possible to remap a single gamepad without affecting all other
    gamepads of the same type.

  * Gamepad `id` is no longer exposed.

  * Added higher-level position functions: `dpadPosition`,
    `leftStickPosition`, `rightStickPosition`.
