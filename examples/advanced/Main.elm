module Main exposing (..)

{-| This (contrived) example shows how to use the Gamepad.Advanced API to:

  - Control when and how the remapping tool is displayed
  - For the sake of example, load and save user mappings inside the URL, you really don't want to do this in a real app
  - Read and use gamepad information directly from the Blob

-}

import Browser exposing (UrlRequest(..))
import Browser.Navigation exposing (Key)
import Gamepad exposing (Gamepad)
import Gamepad.Advanced exposing (Blob, UserMappings)
import GamepadPort
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events
import Url exposing (Url)


-- types


type State
    = Initializing
    | DisplayingGamepads Blob
    | RemappingTool Gamepad.Advanced.Model


type alias Model =
    { state : State
    , userMappings : UserMappings
    , navigationKey : Key
    }


type Msg
    = OnAnimationFrame Blob
    | OnRemappingToolMsg Gamepad.Advanced.Msg
    | OnToggleRemap
    | OnUrlRequest UrlRequest
    | OnUrlChange Url



-- controls


{-| Most of the times, we want to remap only the controls that our application
will actually use, and name them according to the function they will have for
the application.
-}
controlsForASpecificProgram : List ( String, Gamepad.Digital )
controlsForASpecificProgram =
    [ ( "Move Up", Gamepad.LeftStickUp )
    , ( "Move Down", Gamepad.LeftStickDown )
    , ( "Move Left", Gamepad.LeftStickLeft )
    , ( "Move Right", Gamepad.LeftStickRight )
    , ( "Fire", Gamepad.RightTrigger )
    , ( "Jump", Gamepad.LeftTrigger )
    ]


{-| Since this specific example/ can be used also for testing, I think it
is useful to have a complete list of controls with the names of the physical
buttons rather than the name of their effect for a specific application.
-}
allMappableControls : List ( String, Gamepad.Digital )
allMappableControls =
    [ ( "Button A / Cross", Gamepad.A )
    , ( "Button B / Circle", Gamepad.B )
    , ( "Button X / Square", Gamepad.X )
    , ( "Button Y / Triangle", Gamepad.Y )
    , ( "Button Start", Gamepad.Start )
    , ( "Button Back / Select", Gamepad.Back )
    , ( "Logo / Home / Guide", Gamepad.Home )
    , ( "Left Stick: Push Left", Gamepad.LeftStickLeft )
    , ( "Left Stick: Push Right", Gamepad.LeftStickRight )
    , ( "Left Stick: Push Up", Gamepad.LeftStickUp )
    , ( "Left Stick: Push Down", Gamepad.LeftStickDown )
    , ( "Left Stick: Click", Gamepad.LeftStickPress )
    , ( "Left Bumper Button", Gamepad.LeftBumper )
    , ( "Left Trigger / Left Analog Lever", Gamepad.LeftTrigger )
    , ( "Right Stick: Push Left", Gamepad.RightStickLeft )
    , ( "Right Stick: Push Right", Gamepad.RightStickRight )
    , ( "Right Stick: Push Up", Gamepad.RightStickUp )
    , ( "Right Stick: Push Down", Gamepad.RightStickDown )
    , ( "Right Stick: Click", Gamepad.RightStickPress )
    , ( "Right Bumper Button", Gamepad.RightBumper )
    , ( "Right Trigger / Right Analog Lever", Gamepad.RightTrigger )
    , ( "Directional Pad Up", Gamepad.DpadUp )
    , ( "Directional Pad Down", Gamepad.DpadDown )
    , ( "Directional Pad Left", Gamepad.DpadLeft )
    , ( "Directional Pad Right", Gamepad.DpadRight )
    ]


controlsToMap : List ( String, Gamepad.Digital )
controlsToMap =
    allMappableControls



-- URL shenanigans


userMappingsToUrlFragment : UserMappings -> String
userMappingsToUrlFragment userMappings =
    userMappings
        |> Gamepad.Advanced.userMappingsToString
        |> Url.percentEncode


{-| Decode the user mappings from the URL.

If the URL is invalid for any reason, also provide a new Url.

-}
userMappingsFromUrl : Url -> ( UserMappings, Maybe Url )
userMappingsFromUrl url =
    let
        fragmentCurrent =
            Maybe.withDefault "" url.fragment

        userMappings =
            fragmentCurrent
                |> Url.percentDecode
                |> Maybe.withDefault ""
                |> Gamepad.Advanced.userMappingsFromString
                |> Result.withDefault Gamepad.Advanced.emptyUserMappings

        fragmentUpdated =
            userMappingsToUrlFragment userMappings

        -- If the Url had an invalid fragment, we want to update it with a valid one!
        maybeNewUrl =
            if fragmentCurrent == fragmentUpdated then
                Nothing
            else
                Just { url | fragment = Just fragmentUpdated }
    in
    ( userMappings, maybeNewUrl )



-- init


init : () -> Url -> Key -> ( Model, Cmd Msg )
init flags url key =
    updateOnUrlChange
        False
        url
        { state = Initializing
        , userMappings = Gamepad.Advanced.emptyUserMappings
        , navigationKey = key
        }



-- update


noCmd : Model -> ( Model, Cmd msg )
noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnRemappingToolMsg remapMsg ->
            case model.state of
                RemappingTool remapModel ->
                    updateOnRemapMsg remapMsg remapModel model

                _ ->
                    noCmd model

        OnAnimationFrame blob ->
            noCmd { model | state = DisplayingGamepads blob }

        OnToggleRemap ->
            noCmd
                { model
                    | state =
                        case model.state of
                            RemappingTool _ ->
                                Initializing

                            _ ->
                                RemappingTool (Gamepad.Advanced.init controlsToMap)
                }

        OnUrlRequest (Internal url) ->
            updateOnUrlChange True url model

        OnUrlRequest (External url) ->
            ( model, Browser.Navigation.load url )

        OnUrlChange url ->
            updateOnUrlChange False url model


updateOnRemapMsg : Gamepad.Advanced.Msg -> Gamepad.Advanced.Model -> Model -> ( Model, Cmd Msg )
updateOnRemapMsg remapMsg remapModelOld model =
    let
        ( remapModelNew, maybeUpdateUserMappings ) =
            Gamepad.Advanced.update remapMsg remapModelOld
    in
    updateOnMappings maybeUpdateUserMappings { model | state = RemappingTool remapModelNew }


updateOnMappings : Maybe (UserMappings -> UserMappings) -> Model -> ( Model, Cmd a )
updateOnMappings maybeUpdateUserMappings model =
    case maybeUpdateUserMappings of
        -- Gamepad.Advanced.update didn't provide any function to update user mappings
        Nothing ->
            noCmd model

        -- Gamepad.Advanced.update gave us a function to update user mappings, let's do it!
        Just updateMappings ->
            let
                newUserMappings =
                    updateMappings model.userMappings

                newModel =
                    { model | userMappings = newUserMappings }

                cmd =
                    if newUserMappings == model.userMappings then
                        -- userMappings didn't change in any meaningful way,
                        -- no need to change the URL.
                        Cmd.none
                    else
                        -- userMappings changed, let's "save" it in the URL!
                        Browser.Navigation.pushUrl
                            model.navigationKey
                            ("#" ++ userMappingsToUrlFragment newUserMappings)
            in
            ( newModel, cmd )


{-| Update userMappings with the data from the new URL.

If the URL doesn't contain valid a valid user mappings string, replace it.

-}
updateOnUrlChange : Bool -> Url -> Model -> ( Model, Cmd msg )
updateOnUrlChange addToHistory url model =
    let
        ( userMappings, maybeNewUrl ) =
            userMappingsFromUrl url

        updateUrlFunction =
            if addToHistory then
                Browser.Navigation.pushUrl
            else
                Browser.Navigation.replaceUrl

        updateUrlCmd =
            case maybeNewUrl of
                Nothing ->
                    Cmd.none

                Just newUrl ->
                    newUrl
                        |> Url.toString
                        |> updateUrlFunction model.navigationKey
    in
    ( { model | userMappings = userMappings }
    , updateUrlCmd
    )



-- view


boolToString : Bool -> String
boolToString bool =
    case bool of
        True ->
            "True"

        False ->
            "False"


toString : Float -> String
toString =
    String.fromFloat >> String.left 7


recordToString : { x : Float, y : Float } -> String
recordToString { x, y } =
    "{ x: " ++ toString x ++ ", y: " ++ toString y ++ " }"


dpadToString : { x : Int, y : Int } -> String
dpadToString { x, y } =
    String.fromInt x ++ "," ++ String.fromInt y


viewDigital : Gamepad -> ( String, Gamepad.Digital ) -> Html msg
viewDigital gamepad ( name, digital ) =
    li
        []
        [ boolToString (Gamepad.isPressed gamepad digital) ++ " <- " ++ name |> text ]


viewAnalog : String -> Html msg
viewAnalog string =
    li [] [ text string ]


viewGamepad : Gamepad -> Html Msg
viewGamepad gamepad =
    div
        [ style "min-width" "22em" ]
        [ h3
            []
            [ "Gamepad " ++ String.fromInt (Gamepad.getIndex gamepad) |> text ]
        , allMappableControls
            |> List.map (viewDigital gamepad)
            |> ul []
        , [ "Left Stick postion: " ++ recordToString (Gamepad.leftStickPosition gamepad)
          , "Right Stick position: " ++ recordToString (Gamepad.rightStickPosition gamepad)
          , "Dpad position: " ++ dpadToString (Gamepad.dpadPosition gamepad)
          , "Left Trigger (analog)) :" ++ String.fromFloat (Gamepad.value gamepad Gamepad.RightTriggerAnalog)
          , "Right Trigger (analog)) :" ++ String.fromFloat (Gamepad.value gamepad Gamepad.RightTriggerAnalog)
          ]
            |> List.map viewAnalog
            |> ul []
        ]


viewGamepads : Model -> Blob -> Html Msg
viewGamepads model blob =
    let
        views =
            List.map viewGamepad <| Gamepad.Advanced.getGamepads controlsToMap model.userMappings blob
    in
    if List.length views > 0 then
        div
            [ style "display" "flex"
            ]
            views
    else
        div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "border" "1px solid black"
            , style "padding" "16px"
            ]
            [ div [] [ text "Can't find any gamepad! =(" ]
            , div [] [ text "(The browser won't tell me they are there unless you press some button first, so maybe try that)" ]
            ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Gamepad advanced example"
    , body =
        [ div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "padding" "16px"
            ]
            [ case model.state of
                Initializing ->
                    text "Awaiting gamepad blob"

                RemappingTool remapModel ->
                    div
                        []
                        [ Gamepad.Advanced.view model.userMappings remapModel |> Html.map OnRemappingToolMsg
                        , div [] []
                        , div [] []
                        , button
                            [ Html.Events.onClick OnToggleRemap ]
                            [ text "Close Remap Tool" ]
                        ]

                DisplayingGamepads blob ->
                    div
                        []
                        [ div [] []
                        , button
                            [ Html.Events.onClick OnToggleRemap ]
                            [ text "Open Remap Tool" ]
                        , div
                            []
                            [ viewGamepads model blob
                            ]
                        ]
            ]
        ]
    }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        RemappingTool _ ->
            GamepadPort.onBlob (Gamepad.Advanced.onBlob >> OnRemappingToolMsg)

        _ ->
            GamepadPort.onBlob OnAnimationFrame



-- main


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }
