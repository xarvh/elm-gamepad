module Gamepad
    exposing
        ( Gamepad
        , Connection(..)
        , Blob
        , Database
        , RawGamepad
          --
        , getGamepad
        , getGamepadWithDatabase
          -- recognised gamepad
        , aIsPressed
        , bIsPressed
        , xIsPressed
        , yIsPressed
        , startIsPressed
        , backIsPressed
        , guideIsPressed
        , dpadUp
        , dpadDown
        , dpadLeft
        , dpadRight
        , dpadX
        , dpadY
        , leftX
        , leftY
        , leftStickIsPressed
        , leftShoulderIsPressed
        , leftTriggerIsPressed
        , leftTriggerValue
        , rightX
        , rightY
        , rightStickIsPressed
        , rightShoulderIsPressed
        , rightTriggerIsPressed
        , rightTriggerValue
          -- advanced
        , stringToDatabase
        , destinationCodes
        , getFeatures
          -- raw gamepad
        , blobToRawGamepad
        , getId
        , isConnected
        , estimateOriginCode
        )

{-| @docs Gamepad
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Gamepad.DefaultDatabase
import List.Extra
import Regex
import Set exposing (Set)
import Time exposing (Time)


type SourceType
    = Axis
    | Button


type Gamepad
    = Gamepad String RawGamepad


type Connection
    = Disconnected
    | Unrecognised
    | Available Gamepad


{-| TODO constructor should be private
-}
type Database
    = Database (Dict String String)


{-| -}
type alias Blob =
    List (Maybe RawGamepad)


type alias RawButton =
    ( Bool, Float )


{-| This record describes the raw data for a gamepad, as provided directly from the port.

**Do not rely on its internals.**

Instead, manipulate it with the provided functions.

-}
type alias RawGamepad =
    { axes : Array Float
    , buttons : Array RawButton
    , connected : Bool
    , id : String
    , index : Int
    }



-- db helpers


stringToDatabase : String -> Database
stringToDatabase dbAsString =
    let
        stringToTuple dbEntry =
            case String.split ",,," dbEntry of
                [ id, mapping ] ->
                    Just ( id, mapping )

                _ ->
                    Nothing
    in
        dbAsString
            |> String.split "\n"
            |> List.map stringToTuple
            |> List.filterMap identity
            |> Dict.fromList
            |> Database



-- getGamepad helpers


{-| -}
blobToRawGamepad : Int -> Blob -> Maybe RawGamepad
blobToRawGamepad index blob =
    blob
        |> List.filterMap identity
        |> List.Extra.find (\g -> g.index == index)
        |> Maybe.andThen maybeIsConnected


maybeIsConnected : RawGamepad -> Maybe RawGamepad
maybeIsConnected rawGamepad =
    if isConnected rawGamepad then
        Just rawGamepad
    else
        Nothing


stringToInputType : String -> Maybe SourceType
stringToInputType s =
    case s of
        "a" ->
            Just Axis

        "b" ->
            Just Button

        _ ->
            Nothing


maybeToReverse : Maybe String -> Bool
maybeToReverse maybeReverse =
    case maybeReverse of
        Just "-" ->
            True

        _ ->
            False


regexMatchToInputTuple : Regex.Match -> Maybe ( SourceType, Int, Bool )
regexMatchToInputTuple match =
    case match.submatches of
        _ :: maybeReverse :: (Just inputTypeAsString) :: (Just indexAsString) :: _ ->
            Maybe.map3 (,,)
                (inputTypeAsString |> stringToInputType)
                (indexAsString |> String.toInt |> Result.toMaybe)
                (maybeReverse |> maybeToReverse |> Just)

        _ ->
            Nothing


mappingToRawIndex : String -> String -> Maybe ( SourceType, Int, Bool )
mappingToRawIndex destinationCode mapping =
    let
        regex =
            "(^|,)" ++ destinationCode ++ ":(-)?([a-z]?)([0-9]?)(,|$)"
    in
        mapping
            |> Regex.find (Regex.AtMost 1) (Regex.regex regex)
            |> List.head
            |> Maybe.andThen regexMatchToInputTuple



-- input code helpers


axisToButton : Float -> Bool
axisToButton n =
    n > 0.1


buttonToAxis : Bool -> Float
buttonToAxis b =
    if b then
        1
    else
        0


reverseAxis : Bool -> Float -> Float
reverseAxis isReverse n =
    if isReverse then
        -n
    else
        n


isPressed : String -> Gamepad -> Bool
isPressed destinationCode (Gamepad mapping rawGamepad) =
    case mappingToRawIndex destinationCode mapping of
        Nothing ->
            False

        Just ( Axis, index, isReverse ) ->
            Array.get index rawGamepad.axes
                |> Maybe.withDefault 0
                |> reverseAxis isReverse
                |> axisToButton

        Just ( Button, index, isReverse ) ->
            Array.get index rawGamepad.buttons
                |> Maybe.map Tuple.first
                |> Maybe.withDefault False


getValue : String -> Gamepad -> Float
getValue destinationCode (Gamepad mapping rawGamepad) =
    case mappingToRawIndex destinationCode mapping of
        Nothing ->
            0

        Just ( Axis, index, isReverse ) ->
            Array.get index rawGamepad.axes
                |> Maybe.withDefault 0
                |> reverseAxis isReverse

        Just ( Button, index, isReverse ) ->
            Array.get index rawGamepad.buttons
                |> Maybe.map Tuple.first
                |> Maybe.withDefault False
                |> buttonToAxis



-- get gamepad


defaultDatabase : Database
defaultDatabase =
    stringToDatabase Gamepad.DefaultDatabase.asString


getGamepad : Blob -> Int -> Connection
getGamepad =
    getGamepadWithDatabase defaultDatabase


getGamepadWithDatabase : Database -> Blob -> Int -> Connection
getGamepadWithDatabase (Database db) blob index =
    case blobToRawGamepad index blob of
        Nothing ->
            Disconnected

        Just rawGamepad ->
            case Dict.get rawGamepad.id db of
                Nothing ->
                    Unrecognised

                Just mapping ->
                    Available (Gamepad mapping rawGamepad)



-- destination codes


destinationCodes =
    { a = "a"
    , b = "b"
    , x = "x"
    , y = "y"
    , start = "start"
    , back = "back"
    , guide = "guide"
    , leftX = "leftx"
    , leftY = "lefty"
    , leftStick = "leftstick"
    , leftShoulder = "leftshoulder"
    , leftTrigger = "lefttrigger"
    , rightX = "rightx"
    , rightY = "righty"
    , rightStick = "rightstick"
    , rightShoulder = "rightshoulder"
    , rightTrigger = "righttrigger"
    , dpadUp = "dpadup"
    , dpadDown = "dpaddown"
    , dpadLeft = "dpadleft"
    , dpadRight = "dpadright"
    }



-- face buttons


aIsPressed =
    isPressed destinationCodes.a


bIsPressed =
    isPressed destinationCodes.b


xIsPressed =
    isPressed destinationCodes.x


yIsPressed =
    isPressed destinationCodes.y



-- utility


startIsPressed =
    isPressed destinationCodes.start


backIsPressed =
    isPressed destinationCodes.back


guideIsPressed =
    isPressed destinationCodes.guide



-- dpad


dpadUp =
    isPressed destinationCodes.dpadUp


dpadDown =
    isPressed destinationCodes.dpadDown


dpadLeft =
    isPressed destinationCodes.dpadLeft


dpadRight =
    isPressed destinationCodes.dpadRight


dpadX : Gamepad -> Int
dpadX pad =
    if dpadLeft pad then
        -1
    else if dpadRight pad then
        1
    else
        0


dpadY : Gamepad -> Int
dpadY pad =
    if dpadUp pad then
        1
    else if dpadDown pad then
        -1
    else
        0



-- left


leftX =
    getValue destinationCodes.leftX


leftY =
    getValue destinationCodes.leftY


leftStickIsPressed =
    isPressed destinationCodes.leftStick


leftShoulderIsPressed =
    isPressed destinationCodes.leftShoulder


leftTriggerIsPressed =
    isPressed destinationCodes.leftTrigger


leftTriggerValue =
    getValue destinationCodes.leftTrigger



-- right


rightX =
    getValue destinationCodes.rightX


rightY =
    getValue destinationCodes.rightY


rightStickIsPressed =
    isPressed destinationCodes.rightStick


rightShoulderIsPressed =
    isPressed destinationCodes.rightShoulder


rightTriggerIsPressed =
    isPressed destinationCodes.rightTrigger


rightTriggerValue =
    getValue destinationCodes.rightTrigger



--
-- Mapping helpers
--
-- This code is used to get an estimate of the buttons/sticks the user is
-- moving given a time series of RawGamepad states
--


{-| Buttons are always provided as a (isPressed, value) tuple.
The function ignores one and uses only nd always the other.

Is this a good assumption?
Are there cases where both should be considered?

-}
buttonToEstimate : Int -> RawButton -> ( String, Float )
buttonToEstimate originIndex ( isPressed, value ) =
    if isPressed then
        ( "b" ++ toString originIndex, 1 )
    else
        ( "b" ++ toString originIndex, 0 )


axisToEstimate : Int -> Float -> ( String, Float )
axisToEstimate originIndex value =
    if value < 0 then
        ( "-a" ++ toString originIndex, -value )
    else
        ( "a" ++ toString originIndex, value )


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


addRawGamepadToEstimates : RawGamepad -> Dict String Float
addRawGamepadToEstimates rawGamepad =
    let
        axesEstimates =
            Array.indexedMap axisToEstimate rawGamepad.axes

        buttonsEstimates =
            Array.indexedMap buttonToEstimate rawGamepad.buttons
    in
        Array.append axesEstimates buttonsEstimates
            |> Array.foldr addEstimate Dict.empty


estimateThreshold : ( String, Float ) -> Maybe String
estimateThreshold ( code, confidence ) =
    if confidence < 0.5 then
        Nothing
    else
        Just code


{-| The function takes a gamepad state and returns a guess of the origin code
currently activated by the player.
-}
estimateOriginCode : RawGamepad -> Maybe String
estimateOriginCode gamepadState =
    gamepadState
        |> addRawGamepadToEstimates
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> List.head
        |> Maybe.andThen estimateThreshold


isConnected : RawGamepad -> Bool
isConnected =
    .connected


getId : RawGamepad -> String
getId =
    .id


getFeatures : Gamepad -> Set String
getFeatures (Gamepad mapping raw) =
    let
        stripOrigin mappingEntry =
            mappingEntry
                |> String.split ":"
                |> List.head
                |> Maybe.withDefault ""
    in
        mapping
            |> String.split ","
            |> List.map stripOrigin
            |> Set.fromList
