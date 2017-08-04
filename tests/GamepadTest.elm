module GamepadTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Gamepad exposing (Gamepad, RawGamepad, Blob)
import Gamepad.Remap exposing (Outcome(StillOpen, Error, UpdateDatabase))
import Fuzz exposing (Fuzzer)
import Test exposing (Test, describe)


-- Generic Helpers


(&>) =
    flip Fuzz.andThen


repeat : Int -> Fuzzer a -> Fuzzer (List a)
repeat n fuzzer =
    let
        recursive : Int -> List a -> Fuzzer (List a)
        recursive n list =
            if n == 0 then
                Fuzz.constant list
            else
                fuzzer &> \a -> recursive (n - 1) (a :: list)
    in
        recursive n []



-- Gamepad helpers


pressAll : RawGamepad -> RawGamepad
pressAll rawGamepad =
    { rawGamepad
        | axes = Array.map (always 1) rawGamepad.axes
        , buttons = Array.map (always ( True, 1 )) rawGamepad.buttons
    }


pressNone : RawGamepad -> RawGamepad
pressNone rawGamepad =
    { rawGamepad
        | axes = Array.map (always 0) rawGamepad.axes
        , buttons = Array.map (always ( False, 0 )) rawGamepad.buttons
    }


mapBlob : (RawGamepad -> RawGamepad) -> Blob -> Blob
mapBlob f blob =
    blob |> List.map (Maybe.map f)



-- Fuzzers


axesFuzzer : Int -> Fuzzer (Array Float)
axesFuzzer n =
    repeat n (Fuzz.floatRange -1 1) |> Fuzz.map Array.fromList


buttonFuzzer : Fuzzer ( Bool, Float )
buttonFuzzer =
    Fuzz.floatRange 0 1
        |> Fuzz.map (\n -> ( n > 0.01, n ))


buttonsFuzzer : Int -> Fuzzer (Array ( Bool, Float ))
buttonsFuzzer n =
    repeat n buttonFuzzer |> Fuzz.map Array.fromList


standardGamepadFuzzer : Fuzzer RawGamepad
standardGamepadFuzzer =
    let
        makeRawGamepad index axes buttons isConnected id =
            { axes = axes
            , buttons = buttons
            , connected = isConnected
            , id = "(standard) " ++ id ++ " gamepad"
            , index = index
            , mapping = "standard"
            , timestamp = 987
            }
    in
        Fuzz.map5 makeRawGamepad
            Fuzz.int
            (axesFuzzer 4)
            (buttonsFuzzer 16)
            Fuzz.bool
            Fuzz.string


nonStandardGamepadFuzzer : Fuzzer RawGamepad
nonStandardGamepadFuzzer =
    let
        makeRawGamepad index axes buttons isConnected id =
            { axes = axes
            , buttons = buttons
            , connected = isConnected
            , id = "(bleh) " ++ id
            , index = index
            , mapping = ""
            , timestamp = 987
            }
    in
        Fuzz.map2 (,) (Fuzz.intRange 0 8) (Fuzz.intRange 1 32)
            &> \( axesCount, buttonsCount ) ->
                Fuzz.map5 makeRawGamepad
                    Fuzz.int
                    (axesFuzzer axesCount)
                    (buttonsFuzzer buttonsCount)
                    Fuzz.bool
                    Fuzz.string


windows10BuggedGamepadFuzzer : Fuzzer RawGamepad
windows10BuggedGamepadFuzzer =
    let
        makeRawGamepad index isConnected id =
            { axes = [ 0, 0 ] |> Array.fromList
            , buttons = [] |> Array.fromList
            , connected = isConnected
            , id = "(windows 10) " ++ id
            , index = index
            , mapping = ""
            , timestamp = 0
            }
    in
        Fuzz.map3 makeRawGamepad
            Fuzz.int
            Fuzz.bool
            Fuzz.string


blobFuzzer : Fuzzer Gamepad.Blob
blobFuzzer =
    let
        setIndex : Int -> Maybe RawGamepad -> Maybe RawGamepad
        setIndex index maybePad =
            Maybe.map (\pad -> { pad | index = index }) maybePad
    in
        [ standardGamepadFuzzer
        , nonStandardGamepadFuzzer
        , windows10BuggedGamepadFuzzer
        ]
            |> Fuzz.oneOf
            |> Fuzz.maybe
            |> Fuzz.list
            |> Fuzz.map (List.indexedMap setIndex)



-- Gamepad tests


correctlyListsGamepads : Test
correctlyListsGamepads =
    Test.fuzz blobFuzzer "Correctly lists gamepads" <|
        \blob ->
            let
                ( expectedKnownGamepads, expectedUnknownGamepads ) =
                    blob
                        |> List.filterMap identity
                        |> List.filter .connected
                        |> List.filter (\g -> g.timestamp /= 0)
                        |> List.partition (\g -> g.mapping == "standard")

                actualKnownGamepadIds =
                    Gamepad.getGamepads Gamepad.emptyDatabase blob
                        |> List.map Gamepad.getIndex

                actualUnknownGamepadIds =
                    Gamepad.getUnknownGamepads Gamepad.emptyDatabase blob
                        |> List.map Gamepad.unknownGetIndex
            in
                Expect.equal
                    ( actualKnownGamepadIds, actualUnknownGamepadIds )
                    ( List.map .index expectedKnownGamepads, List.map .index expectedUnknownGamepads )


correctlyMapsStandardGamepads : Test
correctlyMapsStandardGamepads =
    Test.fuzz standardGamepadFuzzer "Correctly maps Standard Gamepads" <|
        \rawGamepad ->
            let
                connectedGamepad =
                    { rawGamepad | connected = True }

                blob =
                    [ Just connectedGamepad ]

                -- On a Standard Gamepad, axis[1] corresponds to leftY
                expectedLeftY =
                    connectedGamepad.axes
                        |> Array.get 1
                        -- W3C mandates that inputs have Down in the positive, while the library uses Up as positive.
                        -- This means that all Y values need to be negated.
                        |> Maybe.map negate

                actualLeftY =
                    blob
                        |> Gamepad.getGamepads Gamepad.emptyDatabase
                        |> List.head
                        |> Maybe.map Gamepad.leftY
            in
                Expect.equal actualLeftY expectedLeftY


canOverrideAStandardGamepad : Test
canOverrideAStandardGamepad =
    Test.fuzz standardGamepadFuzzer "Custom maps can override the Standard Gamepad layout" <|
        \rawGamepad ->
            let
                connectedGamepad =
                    { rawGamepad | connected = True }

                blob =
                    [ Just connectedGamepad ]

                -- create a database mapping
                database =
                    (connectedGamepad.id ++ ",,,leftup:a2")
                        |> Gamepad.databaseFromString
                        |> Result.withDefault Gamepad.emptyDatabase

                expectedLeftY =
                    connectedGamepad.axes
                        |> Array.get 2

                actualLeftY =
                    blob
                        |> Gamepad.getGamepads database
                        |> List.head
                        |> Maybe.map Gamepad.leftY
            in
                Expect.equal actualLeftY expectedLeftY


canMapANonStandardGamepad : Test
canMapANonStandardGamepad =
    Test.fuzz standardGamepadFuzzer "Can map a non-standard gamepad" <|
        \rawGamepad ->
            let
                connectedGamepad =
                    { rawGamepad | connected = True }

                blob =
                    [ Just connectedGamepad ]

                -- create a database mapping
                database =
                    (connectedGamepad.id ++ ",,,leftup:b0")
                        |> Gamepad.databaseFromString
                        |> Result.withDefault Gamepad.emptyDatabase

                expectedLeftY =
                    connectedGamepad.buttons
                        |> Array.get 0
                        |> Maybe.map Tuple.second

                actualLeftY =
                    blob
                        |> Gamepad.getGamepads database
                        |> List.head
                        |> Maybe.map Gamepad.leftY
            in
                Expect.equal actualLeftY expectedLeftY



-- Gamepad.Remap tests


chainInputs : Gamepad.Remap.Outcome String -> List Blob -> Gamepad.Remap.Outcome String
chainInputs outcome blobs =
    case ( outcome, blobs ) of
        ( StillOpen model, blob :: bs ) ->
            let
                msg =
                    Gamepad.Remap.testMsg blob

                newOutcome =
                    Gamepad.Remap.update msg model
            in
                chainInputs newOutcome bs

        ( _, _ ) ->
            outcome


canRemapStandardGamepads : Test
canRemapStandardGamepads =
    Test.fuzz blobFuzzer "Can remap standard gamepads" <|
        \blob ->
            case Gamepad.getAllGamepadsAsUnknown blob |> List.head |> Maybe.map Gamepad.unknownGetIndex of
                -- Test remapping a non-existing gamepad index
                Nothing ->
                    let
                        model =
                            Gamepad.Remap.init
                                1
                                [ ( Gamepad.Home, "Home Button" ) ]

                        outcome =
                            chainInputs (StillOpen model) [ blob ]
                    in
                        case outcome of
                            Error message ->
                                Expect.equal message "Gamepad 1 is not connected"

                            _ ->
                                Expect.fail "did not produce an error"

                -- Test remapping a connected gamepad
                Just firstGamepadIndex ->
                    let
                        model =
                            Gamepad.Remap.init
                                firstGamepadIndex
                                [ ( Gamepad.Home, "Home Button" ) ]

                        outcome =
                            chainInputs (StillOpen model)
                                [ blob
                                , mapBlob pressNone blob
                                , mapBlob pressAll blob
                                , mapBlob pressNone blob
                                ]
                    in
                        case outcome of
                            StillOpen model ->
                                Expect.fail "Did not complete remapping"

                            Error message ->
                                Expect.fail message

                            UpdateDatabase updateDb ->
                                Expect.pass
