module Gamepad
    exposing
        ( Gamepad
        , UnknownGamepad
        , Database
          -- port stuff
        , Blob
          --
        , getGamepads
        , getUnknownGamepads
        , emptyDatabase
        , databaseToString
        , databaseFromString
          -- unknown gamepad
        , unknownGetId
        , unknownGetIndex
          -- known gamepad
        , getIndex
        , getFeatures
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
          -- mapping
        , Origin
        , destinationCodes
        , estimateOrigin
        , insertButtonMapInDatabase
        )

{-| @docs Gamepad
-}

import Array exposing (Array)
import Dict exposing (Dict)
import Regex
import Set exposing (Set)
import Time exposing (Time)


{-| exposed types with private constructors
-}
type Gamepad
    = Gamepad String RawGamepad


type UnknownGamepad
    = UnknownGamepad RawGamepad


type Database
    = Database (Dict String ButtonMap)


type Origin
    = Origin Bool OriginType Int


{-| internal types
-}
type OriginType
    = Axis
    | Button


type ButtonMap
    = ButtonMap String


{-| -}
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



-- destination codes


destinationCodes =
    { a = "a"
    , b = "b"
    , x = "x"
    , y = "y"
    , start = "start"
    , back = "back"
    , guide = "guide"
    , leftLeft = "leftleft"
    , leftRight = "leftright"
    , leftUp = "leftup"
    , leftDown = "leftdown"
    , leftStick = "leftstick"
    , leftShoulder = "leftshoulder"
    , leftTrigger = "lefttrigger"
    , rightLeft = "rightleft"
    , rightRight = "rightright"
    , rightUp = "rightup"
    , rightDown = "rightdown"
    , rightStick = "rightstick"
    , rightShoulder = "rightshoulder"
    , rightTrigger = "righttrigger"
    , dpadUp = "dpadup"
    , dpadDown = "dpaddown"
    , dpadLeft = "dpadleft"
    , dpadRight = "dpadright"
    }



-- Adding a ButtonMap to a Database


intToString : Int -> String
intToString =
    toString


{-| If leftUp and leftDown point to different origins, then the normal

    leftY =
        leftUp - leftDown

is perfectly valid.

However if they are on the same origin and that origin is a -1 to +1 axis, the
equality above will yield values between -2 and +2.

This function detects such cases and removes one of the two origins from the
map.

    leftY =
        leftUp

-}
fixAxisCoupling : ( String, String ) -> Dict String Origin -> Dict String Origin
fixAxisCoupling ( code1, code2 ) map =
    case ( Dict.get code1 map, Dict.get code2 map ) of
        ( Just (Origin isReverse1 Axis index1), Just (Origin isReverse2 Axis index2) ) ->
            if index1 == index2 then
                Dict.remove code1 map
            else
                map

        ( _, _ ) ->
            map


fixAllAxesCoupling : Dict String Origin -> Dict String Origin
fixAllAxesCoupling map =
    [ ( destinationCodes.leftLeft, destinationCodes.leftRight )
    , ( destinationCodes.leftUp, destinationCodes.leftDown )
    , ( destinationCodes.rightLeft, destinationCodes.rightRight )
    , ( destinationCodes.rightUp, destinationCodes.rightDown )
    ]
        |> List.foldr fixAxisCoupling map


buttonMap : Dict String Origin -> Result String ButtonMap
buttonMap map =
    let
        hasMinus isReverse =
            if isReverse then
                "-"
            else
                ""

        typeToString originType =
            case originType of
                Axis ->
                    "a"

                Button ->
                    "b"

        originToCode (Origin isReverse originType index) =
            hasMinus isReverse ++ typeToString originType ++ intToString index

        tupleToString ( destinationCode, origin ) =
            destinationCode ++ ":" ++ originToCode origin
    in
        map
            |> fixAllAxesCoupling
            |> Dict.toList
            |> List.map tupleToString
            |> List.sortBy identity
            |> String.join ","
            |> ButtonMap
            |> Ok


insertButtonMapInDatabase : UnknownGamepad -> Dict String Origin -> Database -> Result String Database
insertButtonMapInDatabase unknownGamepad map (Database database) =
    let
        buttonMapToDatabase newButtonMap =
            Dict.insert (unknownGetId unknownGamepad) newButtonMap database |> Database
    in
        buttonMap map
            |> Result.map buttonMapToDatabase



-- Encoding and decoding Databases


emptyDatabase : Database
emptyDatabase =
    Database Dict.empty


buttonMapDivider : String
buttonMapDivider =
    ",,,"


databaseToString : Database -> String
databaseToString (Database database) =
    let
        tupleToString ( gamepadId, ButtonMap map ) =
            gamepadId ++ buttonMapDivider ++ map ++ "\n"
    in
        database
            |> Dict.toList
            |> List.map tupleToString
            |> List.sortBy identity
            |> String.join ""


databaseFromString : String -> Result String Database
databaseFromString databaseAsString =
    let
        stringToTuple dbEntry =
            case String.split buttonMapDivider dbEntry of
                [ id, map ] ->
                    Just ( id, ButtonMap map )

                _ ->
                    Nothing
    in
        databaseAsString
            |> String.split "\n"
            |> List.map stringToTuple
            |> List.filterMap identity
            |> Dict.fromList
            |> Database
            -- TODO: detect and return errors instead of ignoring them silently
            |> Ok



-- Get gamepads


rawGamepadToGamepad : Database -> RawGamepad -> Maybe Gamepad
rawGamepadToGamepad (Database database) rawGamepad =
    case rawGamepad.connected of
        False ->
            Nothing

        True ->
            database
                |> Dict.get rawGamepad.id
                |> Maybe.map (\(ButtonMap buttonMapAsString) -> Gamepad buttonMapAsString rawGamepad)


getGamepads : Database -> Blob -> List Gamepad
getGamepads database blob =
    -- TODO: it might be faster to parse the button maps here, rather than running a regex at every getter
    blob
        |> List.filterMap identity
        |> List.map (rawGamepadToGamepad database)
        |> List.filterMap identity


rawGamepadToUnknownGamepad : Database -> RawGamepad -> Maybe UnknownGamepad
rawGamepadToUnknownGamepad (Database database) rawGamepad =
    case rawGamepad.connected of
        False ->
            Nothing

        True ->
            if Dict.member rawGamepad.id database then
                Nothing
            else
                Just (UnknownGamepad rawGamepad)


getUnknownGamepads : Database -> Blob -> List UnknownGamepad
getUnknownGamepads database blob =
    blob
        |> List.filterMap identity
        |> List.map (rawGamepadToUnknownGamepad database)
        |> List.filterMap identity



-- input code helpers


stringToInputType : String -> Maybe OriginType
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


regexMatchToInputTuple : Regex.Match -> Maybe ( OriginType, Int, Bool )
regexMatchToInputTuple match =
    case match.submatches of
        _ :: maybeReverse :: (Just inputTypeAsString) :: (Just indexAsString) :: _ ->
            Maybe.map3 (,,)
                (inputTypeAsString |> stringToInputType)
                (indexAsString |> String.toInt |> Result.toMaybe)
                (maybeReverse |> maybeToReverse |> Just)

        _ ->
            Nothing


mappingToRawIndex : String -> String -> Maybe ( OriginType, Int, Bool )
mappingToRawIndex destinationCode mapping =
    let
        regex =
            "(^|,)" ++ destinationCode ++ ":(-)?([a-z]?)([0-9]+)(,|$)"
    in
        mapping
            |> Regex.find (Regex.AtMost 1) (Regex.regex regex)
            |> List.head
            |> Maybe.andThen regexMatchToInputTuple


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
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0


getAxis : String -> String -> Gamepad -> Float
getAxis codeNegative codePositive pad =
    (getValue codePositive pad - getValue codeNegative pad)
        |> clamp -1 1



-- Unknown Gamepad getters


unknownGetId : UnknownGamepad -> String
unknownGetId (UnknownGamepad raw) =
    raw.id


unknownGetIndex : UnknownGamepad -> Int
unknownGetIndex (UnknownGamepad raw) =
    raw.index



-- Gamepad getters


getIndex : Gamepad -> Int
getIndex (Gamepad string raw) =
    raw.index


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
    getAxis destinationCodes.leftLeft destinationCodes.leftRight


leftY =
    getAxis destinationCodes.leftDown destinationCodes.leftUp


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
    getAxis destinationCodes.rightLeft destinationCodes.rightRight


rightY =
    getAxis destinationCodes.rightDown destinationCodes.rightUp


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
boolToNumber : Bool -> number
boolToNumber bool =
    if bool then
        1
    else
        0


buttonToEstimate : Int -> RawButton -> ( Origin, Float )
buttonToEstimate originIndex ( isPressed, value ) =
    ( Origin False Button originIndex, boolToNumber isPressed )


axisToEstimate : Int -> Float -> ( Origin, Float )
axisToEstimate originIndex value =
    ( Origin (value < 0) Axis originIndex, abs value )


estimateThreshold : ( Origin, Float ) -> Maybe Origin
estimateThreshold ( origin, confidence ) =
    if confidence < 0.5 then
        Nothing
    else
        Just origin


{-| The function takes a gamepad state and returns a guess of the origin
currently activated by the player.
-}
estimateOrigin : UnknownGamepad -> Maybe Origin
estimateOrigin (UnknownGamepad rawGamepad) =
    let
        axesEstimates =
            Array.indexedMap axisToEstimate rawGamepad.axes

        buttonsEstimates =
            Array.indexedMap buttonToEstimate rawGamepad.buttons
    in
        Array.append axesEstimates buttonsEstimates
            |> Array.toList
            |> List.sortBy Tuple.second
            |> List.reverse
            |> List.head
            |> Maybe.andThen estimateThreshold
