module Gamepad.Remap exposing (..)

import Gamepad
import Gamepad.Visual
import Html exposing (..)
import Time exposing (Time)


{-
   API: Gamepad.Remap
   ------------------

   type alias Model

   type Msg

   type Outcome
     = StillOpen Model
     | Done Config

   view : Model -> Html Msg

   update : Config -> Msg -> Model -> (Outcome, Cmd.msg)

   subscriptions : (TimeAndGamepads -> msg) -> Model -> Sub msg


   -- the remap utility will not ask the user for inputs that are Nothing
   type alias ControlNames =
     { leftStick : Just String
     , rightStick : Just String
     , dPad : Just String
     , leftShoulder : Just String
     , ...
     }

   baseControlNames : ControlNames
   baseControlNames =
     { leftStick = Nothing
     , rightStick = Nothing
     , ...
     }

-}
-- public types


type Config
    = String


type Outcome
    = StillOpen Model
    | Done (Result String Config)


{-| TODO these will be used for partial remaps
type alias ControlNames =
{ leftStick : Just String
, leftShoulder : Just String
, leftTrigger : Just String
, rightStick : Just String
, rightShoulder : Just String
, rightTrigger : Just String
, dPad : Just String
}

baseControlNames : ControlNames
baseControlNames =
{ leftStick = Nothing
, leftShoulder = Nothing
, leftTrigger = Nothing
, rightStick = Nothing
, rightShoulder = Nothing
, rightTrigger = Nothing
, dPad = Nothing
}

-}



--


type alias Model =
    { index : Int
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )



-- init


initFullRemap : Int -> ( Model, Cmd Msg )
initFullRemap index =
    noCmd
        { index = index
        }



-- update


noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Outcome, Cmd Msg )
update msg model =
    noCmd (StillOpen model)



-- view


view : Model -> Html Msg
view model =
    Gamepad.Visual.xbox



-- subscriptions


type alias PortSubscription msg =
    (( Time, Gamepad.Blob ) -> msg) -> Sub msg


subscriptions : PortSubscription Msg -> Model -> Sub Msg
subscriptions portSubscription model =
    portSubscription OnGamepad
