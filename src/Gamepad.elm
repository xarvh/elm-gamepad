module Gamepad
    exposing
        ( Blob
          -- database
        , Database
        , emptyDatabase
        , databaseToString
        , databaseFromString
          -- unknown gamepads
        , UnknownGamepad
        , getUnknownGamepads
        , unknownGetId
        , unknownGetIndex
          -- known gamepads
        , Gamepad
        , getGamepads
        , getIndex
        , aIsPressed
        , bIsPressed
        , xIsPressed
        , yIsPressed
        , startIsPressed
        , backIsPressed
        , homeIsPressed
        , dpadUpIsPressed
        , dpadDownIsPressed
        , dpadLeftIsPressed
        , dpadRightIsPressed
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
        , Destination(..)
        , estimateOrigin
        , buttonMapToUpdateDatabase
        )

{-| A library to make sense of navigator.getGamepads()

You need a JavaScript port to get the return value of `navigator.getGamepads()`
inside Elm.
Within the library, this return value is called a [Blob].

You also need a [Database] of known button maps, but currently there is no
reliable way to create a general database of gamepad button maps for browsers,
so you will have to start with [emptyDatabase] and include a remapping tool to
your app.
You can use the bare-bones tool provided in [Gamepad.Remap] or build your own.
`getUnknownGamepads database blob` will give you a list of connected gamepads
that need to be mapped.

Once you have a database, you can get a list of all recognised and connected
gamepads with `getGamepads database blob`.

To access the information of each gamepad, you can use the button getters
([aIsPressed], [leftX], [rightTriggerValue]...)


# Blob

@docs Blob


# Database

@docs Database, emptyDatabase, databaseToString, databaseFromString


# Unknown Gamepads

@docs UnknownGamepad, getUnknownGamepads, unknownGetId, unknownGetIndex


# Gamepads

@docs Gamepad, getGamepads, getIndex

Depending on the hardware, the drivers and the browser, some input values
will be digital (True or False) and some will be analog (0 to 1 or -1 to 1).

The library hides this complexity and converts the values as necessary.


### Face buttons

@docs aIsPressed, bIsPressed, xIsPressed, yIsPressed


### Utility buttons

@docs startIsPressed, backIsPressed, homeIsPressed


### Digital pad

@docs dpadUpIsPressed, dpadDownIsPressed, dpadLeftIsPressed, dpadRightIsPressed, dpadX, dpadY


### Left thumbstick

@docs leftX, leftY, leftStickIsPressed, leftShoulderIsPressed, leftTriggerIsPressed, leftTriggerValue


### Right thumbstick

@docs rightX, rightY, rightStickIsPressed, rightShoulderIsPressed, rightTriggerIsPressed, rightTriggerValue


# Mapping

@docs Origin, Destination, estimateOrigin, buttonMapToUpdateDatabase

These are the functions used to write the remapping tool in [Gamepad.Remap].
You need them only if instead of [Gamepad.Remap] you want to write your own remapping tool.

A button map associates a raw gamepad input, the [Origin], with a button name, the
[Destination].

The steps to create a button map are roughly:

1.  For every [Destination] your application needs:
      - Ask the user to press or push it.
      - Use [estimateOrigin] to know which [Origin] is being pressed
2.  Feed the list generated above to [buttonMapToUpdateDatabase]

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Regex
import Set exposing (Set)
import Time exposing (Time)


{-| A recognised gamepad, whose buttons mapping was found in the Database.
You can use all control getters to query its state.
-}
type Gamepad
    = Gamepad String RawGamepad


{-| A gamepad that was not found in the Database.
Because of the sheer diversity of gamepads in the wild, there isn't much that
you can reliably do with it.

However, you can remap it and add its entry to the database, so that next time
it will be recognised!

-}
type UnknownGamepad
    = UnknownGamepad RawGamepad


{-| A collection of button maps, by gamepad Id.

If you change the mapping for one gamepad, the mapping will change for all the
gamepads of that type (ie, all the gamepads that share that Id).

-}
type Database
    = Database (Dict String ButtonMap)


{-| An Origin references an input in the javascript [gamepad](https://w3c.github.io/gamepad/)
object.
-}
type Origin
    = Origin Bool OriginType Int


type OriginType
    = Axis
    | Button


type ButtonMap
    = ButtonMap String


{-| A Blob describes the raw return value of navigator.getGamepads()

The whole point of this library is to transform the Blob into something
that is nice to use with Elm.

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
    , timestamp : Float
    }


type Destination
    = A
    | B
    | X
    | Y
    | Start
    | Back
    | Home
    | LeftLeft
    | LeftRight
    | LeftUp
    | LeftDown
    | LeftStick
    | LeftShoulder
    | LeftTrigger
    | RightLeft
    | RightRight
    | RightUp
    | RightDown
    | RightStick
    | RightShoulder
    | RightTrigger
    | DpadUp
    | DpadDown
    | DpadLeft
    | DpadRight



-- destinationToString


destinationToString : Destination -> String
destinationToString destination =
    case destination of
        A ->
            "a"

        B ->
            "b"

        X ->
            "x"

        Y ->
            "y"

        Start ->
            "start"

        Back ->
            "back"

        Home ->
            "home"

        LeftLeft ->
            "leftleft"

        LeftRight ->
            "leftright"

        LeftUp ->
            "leftup"

        LeftDown ->
            "leftdown"

        LeftStick ->
            "leftstick"

        LeftShoulder ->
            "leftshoulder"

        LeftTrigger ->
            "lefttrigger"

        RightLeft ->
            "rightleft"

        RightRight ->
            "rightright"

        RightUp ->
            "rightup"

        RightDown ->
            "rightdown"

        RightStick ->
            "rightstick"

        RightShoulder ->
            "rightshoulder"

        RightTrigger ->
            "righttrigger"

        DpadUp ->
            "dpadup"

        DpadDown ->
            "dpaddown"

        DpadLeft ->
            "dpadleft"

        DpadRight ->
            "dpadright"



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
fixAxisCoupling : ( Destination, Destination ) -> Dict String Origin -> Dict String Origin
fixAxisCoupling ( destination1, destination2 ) map =
    case ( Dict.get (destinationToString destination1) map, Dict.get (destinationToString destination2) map ) of
        ( Just (Origin isReverse1 Axis index1), Just (Origin isReverse2 Axis index2) ) ->
            if index1 == index2 then
                Dict.remove (destinationToString destination1) map
            else
                map

        ( _, _ ) ->
            map


fixAllAxesCoupling : List ( String, Origin ) -> List ( String, Origin )
fixAllAxesCoupling map =
    [ ( LeftLeft, LeftRight )
    , ( LeftUp, LeftDown )
    , ( RightLeft, RightRight )
    , ( RightUp, RightDown )
    ]
        |> List.foldr fixAxisCoupling (Dict.fromList map)
        |> Dict.toList


buttonMap : List ( Destination, Origin ) -> ButtonMap
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

        tupleDestinationToString ( destination, origin ) =
            ( destinationToString destination, origin )

        tupleToString ( destinationAsString, origin ) =
            destinationAsString ++ ":" ++ originToCode origin
    in
        map
            |> List.map tupleDestinationToString
            |> fixAllAxesCoupling
            |> List.map tupleToString
            |> List.sortBy identity
            |> String.join ","
            |> ButtonMap


{-| The function creates a new button map.

The first argument is the gamepad the map is for.

The second argument is the map itself: a List of [Destination]s vs [Origin]s.

The third argument is the [Database] to update.

-}
buttonMapToUpdateDatabase : UnknownGamepad -> List ( Destination, Origin ) -> Database -> Database
buttonMapToUpdateDatabase unknownGamepad map (Database database) =
    Dict.insert (unknownGetId unknownGamepad) (buttonMap map) database |> Database



-- Encoding and decoding Databases


{-| An empty Database.
-}
emptyDatabase : Database
emptyDatabase =
    Database Dict.empty


buttonMapDivider : String
buttonMapDivider =
    ",,,"


{-| Encodes a Database into a String, useful to persist the Database.

    saveDatabaseToLocalStorageCmd =
        gamepadDatabase
            |> databaseToString
            |> LocalStoragePort.set model.gamepadDatabaseKey

-}
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


{-| Decodes a Database from a string, useful to load a persisted Database.

    gamepadDatabase =
        flags.gamepadDatabaseAsString
            |> Gamepad.databaseFromString
            |> Result.withDefault Gamepad.emptyDatabase

-}
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


isConnected : RawGamepad -> Bool
isConnected rawGamepad =
    -- All browsers running under Windows 10 will sometimes throw in a zombie gamepad
    -- object, unrelated to any physical gamepad and never updated.
    -- Since this gamepad has always timestamp == 0, we use this to discard it.
    rawGamepad.connected && rawGamepad.timestamp > 0


rawGamepadToGamepad : Database -> RawGamepad -> Maybe Gamepad
rawGamepadToGamepad (Database database) rawGamepad =
    case isConnected rawGamepad of
        False ->
            Nothing

        True ->
            database
                |> Dict.get rawGamepad.id
                |> Maybe.map (\(ButtonMap buttonMapAsString) -> Gamepad buttonMapAsString rawGamepad)


{-| Get a List of all recognised Gamepads (ie, those that can be found in the Database).
-}
getGamepads : Database -> Blob -> List Gamepad
getGamepads database blob =
    -- TODO: it might be faster to parse the button maps here, rather than running a regex at every getter
    blob
        |> List.filterMap identity
        |> List.map (rawGamepadToGamepad database)
        |> List.filterMap identity


rawGamepadToUnknownGamepad : Database -> RawGamepad -> Maybe UnknownGamepad
rawGamepadToUnknownGamepad (Database database) rawGamepad =
    case isConnected rawGamepad of
        False ->
            Nothing

        True ->
            if Dict.member rawGamepad.id database then
                Nothing
            else
                Just (UnknownGamepad rawGamepad)


{-| Get a List of all gamepads that cannot be found in the Database.
If there are any, you need to run the remapping tool to create a Database
entry for them, otherwise the user won't be able to use them.
-}
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


mappingToRawIndex : Destination -> String -> Maybe ( OriginType, Int, Bool )
mappingToRawIndex destination mapping =
    let
        regex =
            "(^|,)" ++ destinationToString destination ++ ":(-)?([a-z]?)([0-9]+)(,|$)"
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


isPressed : Destination -> Gamepad -> Bool
isPressed destination (Gamepad mapping rawGamepad) =
    case mappingToRawIndex destination mapping of
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


getValue : Destination -> Gamepad -> Float
getValue destination (Gamepad mapping rawGamepad) =
    case mappingToRawIndex destination mapping of
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


getAxis : Destination -> Destination -> Gamepad -> Float
getAxis negativeDestination positiveDestination pad =
    (getValue positiveDestination pad - getValue negativeDestination pad)
        |> clamp -1 1



-- Unknown Gamepad getters


{-| Get the identifier string of an unknown gamepad, as provided by the browser

    unknownGetId unknownGamepad == "Microsoft Corporation. Controller (STANDARD GAMEPAD Vendor: 045e Product: 028e)"

-}
unknownGetId : UnknownGamepad -> String
unknownGetId (UnknownGamepad raw) =
    raw.id


{-| Get the index of an unknown gamepad.
Indexes start from 0.

    unknownGetIndex unknownGamepad == 0

-}
unknownGetIndex : UnknownGamepad -> Int
unknownGetIndex (UnknownGamepad raw) =
    raw.index



-- Gamepad getters


{-| Get the index of a known gamepad.
Indexes start from 0.

    getIndex gamepad == 2

-}
getIndex : Gamepad -> Int
getIndex (Gamepad string raw) =
    raw.index


{-| -}
aIsPressed : Gamepad -> Bool
aIsPressed =
    isPressed A


{-| -}
bIsPressed : Gamepad -> Bool
bIsPressed =
    isPressed B


{-| -}
xIsPressed : Gamepad -> Bool
xIsPressed =
    isPressed X


{-| -}
yIsPressed : Gamepad -> Bool
yIsPressed =
    isPressed Y



-- utility


{-| -}
startIsPressed : Gamepad -> Bool
startIsPressed =
    isPressed Start


{-| -}
backIsPressed : Gamepad -> Bool
backIsPressed =
    isPressed Back


{-| -}
homeIsPressed : Gamepad -> Bool
homeIsPressed =
    isPressed Home



-- dpad


{-| -}
dpadUpIsPressed : Gamepad -> Bool
dpadUpIsPressed =
    isPressed DpadUp


{-| -}
dpadDownIsPressed : Gamepad -> Bool
dpadDownIsPressed =
    isPressed DpadDown


{-| -}
dpadLeftIsPressed : Gamepad -> Bool
dpadLeftIsPressed =
    isPressed DpadLeft


{-| -}
dpadRightIsPressed : Gamepad -> Bool
dpadRightIsPressed =
    isPressed DpadRight


{-| -1 means left, 0 means center, 1 means right
-}
dpadX : Gamepad -> Int
dpadX pad =
    if dpadLeftIsPressed pad then
        -1
    else if dpadRightIsPressed pad then
        1
    else
        0


{-| -1 means down, 0 means center, 1 means up
-}
dpadY : Gamepad -> Int
dpadY pad =
    if dpadUpIsPressed pad then
        1
    else if dpadDownIsPressed pad then
        -1
    else
        0



-- left


{-| -1.0 means full left, 1.0 means full right
-}
leftX : Gamepad -> Float
leftX =
    getAxis LeftLeft LeftRight


{-| -1.0 means full down, 1.0 means full up
-}
leftY : Gamepad -> Float
leftY =
    getAxis LeftDown LeftUp


{-| -}
leftStickIsPressed : Gamepad -> Bool
leftStickIsPressed =
    isPressed LeftStick


{-| -}
leftShoulderIsPressed : Gamepad -> Bool
leftShoulderIsPressed =
    isPressed LeftShoulder


{-| -}
leftTriggerIsPressed : Gamepad -> Bool
leftTriggerIsPressed =
    isPressed LeftTrigger


{-| 0.0 means not pressed, 1.0 means fully pressed
-}
leftTriggerValue : Gamepad -> Float
leftTriggerValue =
    getValue LeftTrigger



-- right


{-| -1.0 means full left, 1.0 means full right
-}
rightX : Gamepad -> Float
rightX =
    getAxis RightLeft RightRight


{-| -1.0 means full down, 1.0 means full up
-}
rightY : Gamepad -> Float
rightY =
    getAxis RightDown RightUp


{-| -}
rightStickIsPressed : Gamepad -> Bool
rightStickIsPressed =
    isPressed RightStick


{-| -}
rightShoulderIsPressed : Gamepad -> Bool
rightShoulderIsPressed =
    isPressed RightShoulder


{-| -}
rightTriggerIsPressed : Gamepad -> Bool
rightTriggerIsPressed =
    isPressed RightTrigger


{-| 0.0 means not pressed, 1.0 means fully pressed
-}
rightTriggerValue : Gamepad -> Float
rightTriggerValue =
    getValue RightTrigger



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


{-| The function makes a guess of the Origin currently activated by the player.
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
