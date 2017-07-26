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


type InputType
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


stringToInputType : String -> Maybe InputType
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


regexMatchToInputTuple : Regex.Match -> Maybe ( InputType, Int, Bool )
regexMatchToInputTuple match =
    case match.submatches of
        _ :: maybeReverse :: (Just inputTypeAsString) :: (Just indexAsString) :: _ ->
            Maybe.map3 (,,)
                (inputTypeAsString |> stringToInputType)
                (indexAsString |> String.toInt |> Result.toMaybe)
                (maybeReverse |> maybeToReverse |> Just)

        _ ->
            Nothing


mappingToRawIndex : String -> String -> Maybe ( InputType, Int, Bool )
mappingToRawIndex inputCode mapping =
    let
        regex =
            "(^|,)" ++ inputCode ++ ":(-)?([a-z]?)([0-9]?)(,|$)"
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
isPressed inputCode (Gamepad mapping rawGamepad) =
    case mappingToRawIndex inputCode mapping of
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
getValue inputCode (Gamepad mapping rawGamepad) =
    case mappingToRawIndex inputCode mapping of
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



-- face buttons


aIsPressed =
    isPressed "a"


bIsPressed =
    isPressed "b"


xIsPressed =
    isPressed "x"


yIsPressed =
    isPressed "y"



-- utility


startIsPressed =
    isPressed "start"


backIsPressed =
    isPressed "back"


guideIsPressed =
    isPressed "guide"



-- dpad


dpadUp =
    isPressed "dpup"


dpadDown =
    isPressed "dpdown"


dpadLeft =
    isPressed "dpleft"


dpadRight =
    isPressed "dpright"


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
        -1
    else if dpadDown pad then
        1
    else
        0



-- left


leftX =
    getValue "leftx"


leftY =
    getValue "lefty"


leftStickIsPressed =
    isPressed "leftstick"


leftShoulderIsPressed =
    isPressed "lefttrigger"


leftTriggerIsPressed =
    isPressed "lefttrigger"


leftTriggerValue =
    getValue "lefttrigger"



-- right


rightX =
    getValue "rightx"


rightY =
    getValue "righty"


rightStickIsPressed =
    isPressed "rightstick"


rightShoulderIsPressed =
    isPressed "righttrigger"


rightTriggerIsPressed =
    isPressed "righttrigger"


rightTriggerValue =
    getValue "righttrigger"
