Using Vendor and Product codes
==============================

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
