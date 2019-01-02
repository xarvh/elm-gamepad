module Gamepad.Private exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)


-- Gamepad


type Gamepad
    = Gamepad Mapping GamepadFrame GamepadFrame



-- Types populated by gamepadPort.js


type alias GamepadFrame =
    { axes : Array Float
    , buttons : Array ( Bool, Float )
    , id : String
    , index : Int
    , mapping : String
    }


type alias Environment =
    { userMappings : String
    , languages : List String
    }


emptyEnvironment : Environment
emptyEnvironment =
    { userMappings = "{}"
    , languages = []
    }


type alias BlobFrame =
    { gamepads : List GamepadFrame
    , timestamp : Float
    }


emptyBlobFrame : BlobFrame
emptyBlobFrame =
    { timestamp = 0
    , gamepads = []
    }


type alias Blob =
    ( BlobFrame, BlobFrame, Environment )


emptyBlob : Blob
emptyBlob =
    ( emptyBlobFrame, emptyBlobFrame, emptyEnvironment )



-- Mapping


type OriginType
    = Axis
    | Button


type alias Origin =
    { isReverse : Bool
    , type_ : OriginType
    , index : Int
    }


type alias Mapping =
    Dict String (List Origin)



-- Misc


boolToNumber : Bool -> number
boolToNumber bool =
    if bool then
        1
    else
        0


axisToButton : Float -> Bool
axisToButton n =
    n > 0.6


buttonToAxis : Bool -> Float
buttonToAxis =
    boolToNumber
