module Gamepad.Remap exposing (..)

import Array
import Dict exposing (Dict)
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
--


type Status
    = Idle String
    | Guessing Time (List Gamepad.Blob)


type alias Model =
    { index : Int
    , timeline : List Gamepad.RawGamepad
    , best : ( String, Float )
    }


type Msg
    = OnGamepad ( Time, Gamepad.Blob )



--


type Target
    = Stick
    | Button
    | Dpad


type alias CurrentTarget =
    { name : String
    }



-- Guess
--
-- This code is used to get an estimate of the buttons/sticks the user is
-- moving given a time series of RawGamepad states


axisToEstimate index value =
    ( "a" ++ toString index, value * value )


{-| TODO: use bool?
-}
buttonToEstimate index ( bool, value ) =
    ( "b" ++ toString index, value * value )


addEstimate : ( String, Float ) -> Dict String Float -> Dict String Float
addEstimate ( code, weight ) oldEstimates =
    let
        newWeight =
            oldEstimates
                |> Dict.get code
                |> Maybe.withDefault 0
                |> (+) weight
    in
        Dict.insert code newWeight oldEstimates


addRawGamepadToEstimates : Gamepad.RawGamepad -> Dict String Float -> Dict String Float
addRawGamepadToEstimates rawGamepad estimates =
    let
        axesEstimates =
            Array.indexedMap axisToEstimate rawGamepad.axes

        buttonsEstimates =
            Array.indexedMap buttonToEstimate rawGamepad.buttons
    in
        Array.append axesEstimates buttonsEstimates
            |> Array.foldr addEstimate estimates


guessButton : List Gamepad.RawGamepad -> ( String, Float )
guessButton timeline =
    timeline
        |> List.foldr addRawGamepadToEstimates Dict.empty
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ("nothing captured", 0)



-- init


initFullRemap : Int -> ( Model, Cmd Msg )
initFullRemap index =
    noCmd
        { index = index
        , timeline = []
        , best = ( "", 0 )
        }



-- update


noCmd model =
    ( model, Cmd.none )


onGamepad : Time -> Gamepad.Blob -> Model -> Model
onGamepad dt blob model =
    case Gamepad.blobToRawGamepad model.index blob of
        Nothing ->
            { model | timeline = [], best = ( "not connected", 0 ) }

        Just rawGamepad ->
            let
                timeline =
                    model.timeline
                        |> (::) rawGamepad
                        |> List.take 20

                best =
                    guessButton timeline
            in
                { model | timeline = timeline, best = best }


update : Msg -> Model -> ( Outcome, Cmd Msg )
update msg model =
    case msg of
        OnGamepad ( dt, blob ) ->
            noCmd <| StillOpen <| onGamepad dt blob model



-- view


view : Model -> Html Msg
view model =
    text <| toString <| model.best



-- subscriptions


type alias PortSubscription msg =
    (( Time, Gamepad.Blob ) -> msg) -> Sub msg


subscriptions : PortSubscription Msg -> Model -> Sub Msg
subscriptions portSubscription model =
    portSubscription OnGamepad
