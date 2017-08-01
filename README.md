Elm Gamepads
============

[See a running version of examples/Main.elm](https://xarvh.github.io/elm-gamepad/).


TODO


What's the problem with Browsers+Gamepads?
==========================================

The [w3c spec](https://www.w3.org/TR/gamepad/) is still very fresh and different browsers and different browser
will map the same gamepad differently; for example, Firefox considers the shoulder buttons of my Xbox360
gamepads as axes, while Chrome detects them as Buttons. The two browsers even produce a different DOMString.

As it is, using Vendor/Product codes is completely useless.
Only exact matches of Gamepad.id should be considered.


### SDL

https://github.com/gabomdq/SDL_GameControllerDB/blob/master/gamecontrollerdb.txt

The code represents four big-endian, 32-bit integers
03000000 5e040000 8e020000 04010000

The second integer is the Vendor id, little-endian 045e
The third integer is the Product id, little-endian 028e



### Firefox
"045e-028e-Microsoft X-Box 360 pad"
$vendor-$product-$name




### Chromium, Opera
"Microsoft Corporation. Controller (STANDARD GAMEPAD Vendor: 045e Product: 028e)"
$name (?? Vendor: $vendor Product: $product)
