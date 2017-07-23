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
   ISSUE:
     digital pad is represented sometimes as two axis, sometimes as four buttons.
     How do we express this in the db?

     one possiblity: "dpX:b11b12"





    API: Gamepad
    ------------

    features : Gamepad -> Set Feature

    leftShoulderIsPressed : Gamepad -> Bool
    leftShoulderValue : Gamepad -> Float
-}
-- TODO constructor should be private


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


type ControlInput
    = Axis Int
    | Button Int


mappingToRawIndex : String -> String -> Maybe ControlInput
mappingToRawIndex inputCode mapping =
    let
        regex =
            "(^|,)" ++ inputCode ++ ":([a-z]?)([0-9]?)(,|$)"

        regexMatchToControlInput : Regex.Match -> Maybe ControlInput
        regexMatchToControlInput match =
            case match.submatches of
                _ :: (Just "b") :: (Just index) :: _ ->
                    index
                        |> String.toInt
                        |> Result.toMaybe
                        |> Maybe.map Button

                _ :: (Just "a") :: (Just index) :: _ ->
                    index
                        |> String.toInt
                        |> Result.toMaybe
                        |> Maybe.map Axis

                _ ->
                    Nothing
    in
        mapping
            |> Regex.find (Regex.AtMost 1) (Regex.regex regex)
            |> List.head
            |> Maybe.andThen regexMatchToControlInput



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


isPressed : String -> Gamepad -> Bool
isPressed inputCode (Gamepad mapping rawGamepad) =
    case mappingToRawIndex inputCode mapping of
        Nothing ->
            False

        Just (Axis index) ->
            Array.get index rawGamepad.axes
                |> Maybe.withDefault 0
                |> axisToButton

        Just (Button index) ->
            Array.get index rawGamepad.buttons
                |> Maybe.map Tuple.first
                |> Maybe.withDefault False


getValue : String -> Gamepad -> Float
getValue inputCode (Gamepad mapping rawGamepad) =
    case mappingToRawIndex inputCode mapping of
        Nothing ->
            0

        Just (Axis index) ->
            Array.get index rawGamepad.axes
                |> Maybe.withDefault 0

        Just (Button index) ->
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
    case blob |> List.filterMap identity |> List.Extra.find (\g -> g.index == index) of
        Nothing ->
            Disconnected

        Just rawGamepad ->
            if not rawGamepad.connected then
                Disconnected
            else
                -- TODO search first in the custom dict
                case Dict.get rawGamepad.id db of
                    Nothing ->
                        Unrecognised

                    Just mapping ->
                        Available (Gamepad mapping rawGamepad)


aIsPressed =
    isPressed "a"


bIsPressed =
    isPressed "b"


xIsPressed =
    isPressed "x"


yIsPressed =
    isPressed "y"


startIsPressed =
    isPressed "start"


backIsPressed =
    isPressed "back"


guideIsPressed =
    isPressed "guide"


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
