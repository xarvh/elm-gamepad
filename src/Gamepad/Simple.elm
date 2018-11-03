module Gamepad.Simple
    exposing
        ( Config
        , FrameStuff
        , Program
        , application
        , basicControls
        , document
        , element
        , sandbox
        )

{-| This is the easiest way to add gamepads to an app.

It provides four functions: `sandbox`, `element`, `document`, `application`
that replace those in the [elm/browser](https://package.elm-lang.org/packages/elm/browser/latest/Browser)
package.

These functions require an additional [Config](#Config) argument, which tells them
what to do with gamepads.

    import Gamepad
    import Gamepad.Simple
    import GamepadPort


    type Msg
        = OnAnimationFrame Gamepad.Simple.FrameStuff


    config : Gamepad.Simple.Config Msg
    config =
        { onAnimationFrame = OnAnimationFrame
        , onBlob = GamepadPort.onBlob
        , saveToLocalStorage = GamepadPort.saveToLocalStorage
        , controls =
            [ ( "Move LEFT", Gamepad.LeftStickLeft )
            , ( "Move RIGHT", Gamepad.LeftStickRight )
            , ( "Move UP", Gamepad.LeftStickUp )
            , ( "Move DOWN", Gamepad.LeftStickDown )

            --
            , ( "Aim LEFT", Gamepad.RightStickLeft )
            , ( "Aim RIGHT", Gamepad.RightStickRight )
            , ( "Aim UP", Gamepad.RightStickUp )
            , ( "Aim DOWN", Gamepad.RightStickDown )

            --
            , ( "FIRE", Gamepad.RightTrigger )
            , ( "Alt FIRE", Gamepad.RightBumper )
            , ( "Transform", Gamepad.A )
            , ( "Rally", Gamepad.B )
            , ( "Menu", Gamepad.Start )
            ]
        }


    main =
        Gamepad.Simple.sandbox
            config
            { init = init
            , view = view
            , update = update
            }


    update msg model =
        case msg of
            OnAnimationFrame { gamepads, timestamp, dt } ->
                ...

You will need to manually add port code. See [Adding Ports](https://package.elm-lang.org/packages/xarvh/elm-gamepad/latest/#adding-ports)
for how to do it.

The remapping tool can be toggled with the `Escape` key, but will pop up
automatically when the user tries to use an unrecognized gamepad.

While the remapping tool is open, the wrapped app will continue to receive its
own messages normally, but will NOT receive `onAnimationFrame`.

User mappings are saved to localStorage.

@docs Program, Config, FrameStuff

@docs sandbox, element, document, application

@docs basicControls

-}

import Browser exposing (Document, UrlRequest)
import Browser.Events
import Browser.Navigation exposing (Key)
import Gamepad exposing (Digital(..), Gamepad)
import Gamepad.Advanced as Advanced exposing (Blob, UserMappings)
import Gamepad.Private as Private
import Gamepad.Translations exposing (Translation)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder)
import Platform
import Time exposing (Posix)
import Url exposing (Url)


{-| A Program wrapped by this module
-}
type alias Program flags model msg =
    Platform.Program flags (Model model) (Msg msg)


{-| The additional parameters needed to create a wrapped Program

  - `onBlob` should be always set to `GamepadPort.onBlob`
  - `saveToLocalStorage` should be always set to `GamepadPort.saveToLocalStorage`
  - `onAnimationFrame` will give you timing and gamepad information
  - `controls` specifies which controls you want and which name to use when asking the user to configure them

-}
type alias Config msg =
    { onBlob : (Blob -> Msg msg) -> Sub (Msg msg)
    , saveToLocalStorage : String -> Cmd Never
    , onAnimationFrame : FrameStuff -> msg
    , controls : List ( String, Gamepad.Digital )
    }


{-| Gamepad polling should happen at every animation frame, so timing information
and gamepad state information arrive together in this record.

Use the functions in the [Gamepad](https://package.elm-lang.org/packages/xarvh/elm-gamepad/latest/Gamepad)
module to make sense of the `Gamepad` objects.

The `timestamp` and `dt` fields are exactly what you would expect if you were using
[Browser.Events.onAnimationFrame](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrame)
and
[Browser.Events.onAnimationFrameDelta](https://package.elm-lang.org/packages/elm/browser/latest/Browser-Events#onAnimationFrameDelta)
respectively.

-}
type alias FrameStuff =
    { gamepads : List Gamepad
    , timestamp : Posix
    , dt : Float
    }



-- Controls


{-| If you don't know yet which controls to use for your game, you can use
this list: `Gamepad.Simple` will use the list to ask the user to configure
the Left Stick and two buttons, A and B.

    basicControls =
        [ ( "Up", Gamepad.LeftStickUp )
        , ( "Down", Gamepad.LeftStickDown )
        , ( "Left", Gamepad.LeftStickLeft )
        , ( "Right", Gamepad.LeftStickRight )
        , ( "A", Gamepad.A )
        , ( "B", Gamepad.B )
        ]

This means that inside your app, you will be able to use:

  - `leftStickPosition`
  - `isPressed`, `wasClicked`, `wasReleased` with `LeftStickUp`, `LeftStickDown`, `LeftStickLeft`, `LeftStickRight`, `A`, `B`
  - `value` with `LeftX`, `LeftY`

-}
basicControls : List ( String, Gamepad.Digital )
basicControls =
    [ ( "Up", Gamepad.LeftStickUp )
    , ( "Down", Gamepad.LeftStickDown )
    , ( "Left", Gamepad.LeftStickLeft )
    , ( "Right", Gamepad.LeftStickRight )
    , ( "A", Gamepad.A )
    , ( "B", Gamepad.B )
    ]



-- Program constructors


type alias NormalizedUpdate model msg =
    msg -> model -> ( model, Cmd msg )


type alias ElementView model msg =
    model -> Html msg


type alias DocumentView model msg =
    model -> Document msg


type alias NormalizedSubscriptions model msg =
    model -> Sub msg


{-| Same as [Browser.sandbox](https://package.elm-lang.org/packages/elm/browser/latest/Browser#sandbox)
-}
sandbox :
    Config msg
    ->
        { init : model
        , view : model -> Html msg
        , update : msg -> model -> model
        }
    -> Program () model msg
sandbox config child =
    let
        normalizedUpdate : NormalizedUpdate model msg
        normalizedUpdate msg model =
            ( child.update msg model, Cmd.none )

        normalizedSubscriptions : NormalizedSubscriptions model msg
        normalizedSubscriptions model =
            Sub.none
    in
    Browser.element
        { init = \flags -> init ( child.init, Cmd.none )
        , update = update config normalizedUpdate
        , view = viewElement config child.view
        , subscriptions = subscriptions config normalizedSubscriptions
        }


{-| Same as [Browser.element](https://package.elm-lang.org/packages/elm/browser/latest/Browser#element)
-}
element :
    Config msg
    ->
        { init : flags -> ( model, Cmd msg )
        , view : model -> Html msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        }
    -> Program flags model msg
element config child =
    Browser.element
        { init = child.init >> init
        , view = viewElement config child.view
        , update = update config child.update
        , subscriptions = subscriptions config child.subscriptions
        }


{-| Same as [Browser.document](https://package.elm-lang.org/packages/elm/browser/latest/Browser#document)
-}
document :
    Config msg
    ->
        { init : flags -> ( model, Cmd msg )
        , view : model -> Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        }
    -> Program flags model msg
document config child =
    Browser.document
        { init = child.init >> init
        , view = viewDocument config child.view
        , update = update config child.update
        , subscriptions = subscriptions config child.subscriptions
        }


{-| Same as [Browser.application](https://package.elm-lang.org/packages/elm/browser/latest/Browser#application)
-}
application :
    Config msg
    ->
        { init : flags -> Url -> Key -> ( model, Cmd msg )
        , view : model -> Document msg
        , update : msg -> model -> ( model, Cmd msg )
        , subscriptions : model -> Sub msg
        , onUrlRequest : UrlRequest -> msg
        , onUrlChange : Url -> msg
        }
    -> Program flags model msg
application config child =
    Browser.application
        { init = \flags url key -> child.init flags url key |> init
        , view = viewDocument config child.view
        , update = update config child.update
        , subscriptions = subscriptions config child.subscriptions
        , onUrlRequest = child.onUrlRequest >> OnChildMsg
        , onUrlChange = child.onUrlChange >> OnChildMsg
        }



-- TEA


type alias Model model =
    { childModel : model
    , state : State
    }


type State
    = Initialising
    | Running RunningModel (Maybe Advanced.Model)


type alias RunningModel =
    { userMappings : UserMappings
    , unmappedGamepads : Int
    , languages : List String
    , ignoreUnconfiguredGamepads : Bool
    }


type Msg msg
    = OnChildMsg msg
    | OnBlob Blob
    | OnAdvancedMsg Advanced.Msg
    | OnToggleRemappingTool



-- Init


init : ( model, Cmd msg ) -> ( Model model, Cmd (Msg msg) )
init ( childModel, childCmd ) =
    let
        model =
            { childModel = childModel
            , state = Initialising
            }

        cmd =
            Cmd.map OnChildMsg childCmd
    in
    ( model, cmd )



-- Update


update : Config msg -> NormalizedUpdate model msg -> Msg msg -> Model model -> ( Model model, Cmd (Msg msg) )
update config childUpdate msg model =
    case msg of
        OnChildMsg childMsg ->
            updateWithChildMsg childUpdate childMsg model

        OnBlob blob ->
            updateOnBlob config childUpdate blob model

        OnAdvancedMsg advancedMsg ->
            case model.state of
                Running runningModel (Just advancedModel) ->
                    updateWithAdvancedMsg config runningModel advancedMsg advancedModel model

                _ ->
                    noCmd model

        OnToggleRemappingTool ->
            case model.state of
                Initialising ->
                    noCmd model

                Running runningModel (Just advancedModel) ->
                    noCmd { model | state = Running { runningModel | ignoreUnconfiguredGamepads = True } Nothing }

                Running runningModel Nothing ->
                    noCmd { model | state = Running runningModel <| Just <| Advanced.init config.controls }


noCmd model =
    ( model, Cmd.none )


updateWithChildMsg : NormalizedUpdate model msg -> msg -> Model model -> ( Model model, Cmd (Msg msg) )
updateWithChildMsg childUpdate childMsg model =
    childUpdate childMsg model.childModel
        |> Tuple.mapBoth (\m -> { model | childModel = m }) (Cmd.map OnChildMsg)


updateWithAdvancedMsg : Config msg -> RunningModel -> Advanced.Msg -> Advanced.Model -> Model model -> ( Model model, Cmd a )
updateWithAdvancedMsg config runningModelOld advancedMsg advancedModelOld model =
    let
        ( advancedModelNew, maybeUpdateUserMappings ) =
            Advanced.update advancedMsg advancedModelOld

        ( runningModelNew, cmd ) =
            case maybeUpdateUserMappings of
                Nothing ->
                    ( runningModelOld, Cmd.none )

                Just updateUserMappings ->
                    let
                        um =
                            updateUserMappings runningModelOld.userMappings

                        cc =
                            um
                                |> Advanced.userMappingsToString
                                |> config.saveToLocalStorage
                                |> Cmd.map never
                    in
                    ( { runningModelOld | userMappings = um }, cc )
    in
    ( { model | state = Running runningModelNew (Just advancedModelNew) }, cmd )


updateOnBlob : Config msg -> NormalizedUpdate model msg -> Blob -> Model model -> ( Model model, Cmd (Msg msg) )
updateOnBlob config childUpdate blob model =
    case model.state of
        Initialising ->
            let
                ( currentFrame, previousFrame, environment ) =
                    blob

                userMappings =
                    environment.userMappings
                        |> Advanced.userMappingsFromString
                        |> Result.withDefault Advanced.emptyUserMappings

                runningModel =
                    { userMappings = userMappings
                    , languages = environment.languages
                    , unmappedGamepads = 0
                    , ignoreUnconfiguredGamepads = False
                    }
            in
            noCmd { model | state = Running runningModel Nothing }

        Running runningModelOld maybeAdvancedModel ->
            let
                runningModelNew =
                    { runningModelOld | unmappedGamepads = Advanced.unmappedGamepads runningModelOld.userMappings blob }
            in
            case maybeAdvancedModel of
                Just advancedModel ->
                    updateWithAdvancedMsg config runningModelNew (Advanced.onBlob blob) advancedModel model

                Nothing ->
                    if runningModelNew.unmappedGamepads > 0 && not runningModelNew.ignoreUnconfiguredGamepads then
                        noCmd { model | state = Running runningModelNew <| Just <| Advanced.init config.controls }
                    else
                        let
                            frameStuff =
                                { gamepads = Advanced.getGamepads config.controls runningModelNew.userMappings blob
                                , timestamp = Advanced.animationFrameTimestamp blob
                                , dt = Advanced.animationFrameDelta blob
                                }

                            childMsg =
                                config.onAnimationFrame frameStuff
                        in
                        updateWithChildMsg childUpdate childMsg model



-- View


viewElement : Config msg -> ElementView model msg -> Model model -> Html (Msg msg)
viewElement config childView model =
    let
        el =
            childView model.childModel
                |> Html.map OnChildMsg
    in
    case model.state of
        Initialising ->
            el

        Running runningModel Nothing ->
            el

        Running runningModel (Just advancedModel) ->
            viewAdvanced runningModel el (Advanced.view runningModel.userMappings advancedModel)


viewDocument : Config msg -> DocumentView model msg -> Model model -> Document (Msg msg)
viewDocument config childView model =
    let
        doc =
            childView model.childModel

        body =
            doc.body |> List.map (Html.map OnChildMsg)
    in
    { title = doc.title
    , body =
        case model.state of
            Initialising ->
                body

            Running runningModel Nothing ->
                body

            Running runningModel (Just advancedModel) ->
                [ viewAdvanced runningModel (div [] body) (Advanced.view runningModel.userMappings advancedModel)
                ]
    }


viewAdvanced : RunningModel -> Html (Msg msg) -> Html Advanced.Msg -> Html (Msg msg)
viewAdvanced runningModel child remapToolView =
    let
        unmapped =
            runningModel.unmappedGamepads

        translation =
            Gamepad.Translations.pickTranslation runningModel.languages Gamepad.Translations.allTranslations

        ( tr_pressThe, tr_esc, tr_keyToToggle ) =
            translation.pressTheEscKeyToToggle
    in
    div
        [ class "elm-gamepad-root"
        ]
        [ child
        , node "style" [] [ text cssStyle ]
        , div
            [ class "elm-gamepad-opaque-background"
            ]
            []
        , div
            [ class "elm-gamepad-modal-container"
            ]
            [ div
                [ class "elm-gamepad-modal"
                ]
                [ span
                    [ class "elm-gamepad-close"
                    , onClick OnToggleRemappingTool
                    ]
                    [ text "×"
                    ]
                , div
                    [ class "elm-gamepad-escape-message"
                    ]
                    [ text <| tr_pressThe ++ " "
                    , span
                        [ class "elm-gamepad-escape-key"
                        ]
                        [ text tr_esc
                        ]
                    , text <| " " ++ tr_keyToToggle
                    ]

                -- actual remap tool
                , remapToolView
                    |> Html.map OnAdvancedMsg
                ]
            ]
        ]


cssStyle =
    """
.elm-gamepad-root {
  position: relative;
}

.elm-gamepad-opaque-background {
  position: absolute;
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  background-color: gray;
  opacity: 0.8;
}

.elm-gamepad-modal-container {
  position: absolute;
  top: 0;
  left: 0;
  width: 100vw;
  height: 100vh;
  display: flex;
  align-items: center;
  justify-content: center;
  flex-direction: column;
}

.elm-gamepad-modal {
  position: relative;
  border: 0.2em solid black;
  background-color: white;
  padding: 1em;
  opacity: 1;
}

.elm-gamepad-close {
  position: absolute;
  top: 0;
  right: 0.25em;
  font-weight: bold;
  cursor: pointer;
}

.elm-gamepad-escape-message {
  margin-bottom: 1em;
}

.elm-gamepad-escape-key {
  border: 1px solid gray;
  padding: 0.25em;
  border-radius: 0.25em;
  background-color: lightgray;
  box-shadow: 0px 2px 1px 0px gray;
}
"""



-- Subscriptions


escapeDecoder : msg -> Decoder msg
escapeDecoder msg =
    let
        isEscape keyName =
            if keyName == "Escape" then
                Json.Decode.succeed msg
            else
                Json.Decode.fail ""
    in
    Json.Decode.string
        |> Json.Decode.field "key"
        |> Json.Decode.andThen isEscape


subscriptions : Config msg -> NormalizedSubscriptions model msg -> Model model -> Sub (Msg msg)
subscriptions config childSubscriptions model =
    Sub.batch
        [ config.onBlob OnBlob
        , Browser.Events.onKeyUp (escapeDecoder OnToggleRemappingTool)
        , childSubscriptions model.childModel |> Sub.map OnChildMsg
        ]
