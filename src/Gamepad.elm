module Gamepad exposing (..)

{-| @docs Gamepad
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Gamepad.DefaultDb
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Regex
import Time exposing (Time)


{-
   TODO:
     * features : Gamepad -> Set Feature
-}


{-| TODO constructor should be private
-}
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


{-| Can't really make this private, since it must come from a port
-}
type alias Blob =
    List (Maybe RawGamepad)


type alias RawButton =
    ( Bool, Float )


type alias RawGamepad =
    { axes : Array Float
    , buttons : Array RawButton
    , connected : Bool
    , id : String
    , index : Int
    }



-- db helpers


defaultDb : Database
defaultDb =
    dbDecoder Gamepad.DefaultDb.defaultDbAsString


dbDecoder : String -> Database
dbDecoder dbAsString =
    let
        stringToTuple dbEntry =
            case String.split "```" dbEntry of
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


type SourceType
    = Axis
    | Button


{-| TODO this function should be visible by Gamepad.Remap, but not elsewhere
-}
blobToRawGamepad : Int -> Blob -> Maybe RawGamepad
blobToRawGamepad index blob =
    blob
        |> List.filterMap identity
        |> List.Extra.find (\g -> g.index == index)
        |> Maybe.andThen isConnected


isConnected : RawGamepad -> Maybe RawGamepad
isConnected rawGamepad =
    if rawGamepad.connected then
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



-- public functions


getGamepad : Blob -> Int -> Connection
getGamepad =
    getGamepadWithDb defaultDb


getGamepadWithDb : Database -> Blob -> Int -> Connection
getGamepadWithDb (Database db) blob index =
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
