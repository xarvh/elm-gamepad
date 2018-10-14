module Main exposing (..)

import Dict exposing (Dict)
import Gamepad exposing (Gamepad)
import Gamepad.Simple
import GamepadPort
import Html exposing (..)
import Html.Attributes exposing (style)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)


-- types


type alias Player =
    { gamepadIndex : Int
    , position : Vec2
    , size : Float
    }


type alias Model =
    { players : List Player
    }


type Msg
    = OnAnimationFrame Gamepad.Simple.FrameStuff



-- button mapping


type alias PlayerInput =
    { grow : Bool
    , shrink : Bool
    , speed : Vec2
    }


neutralPlayerInput : PlayerInput
neutralPlayerInput =
    { grow = False
    , shrink = False
    , speed = vec2 0 0
    }


{-| We want to remap only the controls that our application will actually use
and name them according to the function they will have for the application.
-}
controls =
    [ ( "Move Up", Gamepad.LeftStickUp )
    , ( "Move Down", Gamepad.LeftStickDown )
    , ( "Move Left", Gamepad.LeftStickLeft )
    , ( "Move Right", Gamepad.LeftStickRight )
    , ( "Grow", Gamepad.A )
    , ( "Shrink", Gamepad.B )
    ]


gamepadToInput : Gamepad -> ( Int, PlayerInput )
gamepadToInput gamepad =
    let
        index : Int
        index =
            Gamepad.getIndex gamepad

        input : PlayerInput
        input =
            { grow = Gamepad.isPressed gamepad Gamepad.A
            , shrink = Gamepad.wasClicked gamepad Gamepad.B
            , speed = Vec2.fromRecord (Gamepad.leftStickPosition gamepad)
            }
    in
    ( index, input )



-- game state update


updatePlayersState : Float -> List Gamepad -> List Player -> List Player
updatePlayersState dt gamepads oldPlayers =
    let
        inputsByGamepadIndex : Dict Int PlayerInput
        inputsByGamepadIndex =
            gamepads
                |> List.map gamepadToInput
                |> Dict.fromList

        getInput : Player -> PlayerInput
        getInput player =
            Dict.get player.gamepadIndex inputsByGamepadIndex
                |> Maybe.withDefault neutralPlayerInput

        updatePlayer : Player -> Player
        updatePlayer player =
            updatePlayerState dt (getInput player) player
    in
    List.map updatePlayer oldPlayers


updatePlayerState : Float -> PlayerInput -> Player -> Player
updatePlayerState dt input player =
    let
        movementSpeed =
            0.04

        growSpeed =
            0.01

        dp =
            Vec2.scale (dt * movementSpeed) input.speed

        position =
            Vec2.add player.position dp

        size =
            if input.shrink then
                5
            else if input.grow then
                player.size + growSpeed * dt
            else
                player.size
    in
    { player | size = size, position = position }



-- init


init : Model
init =
    { players =
        [ { gamepadIndex = 1
          , position = vec2 70 20
          , size = 10
          }
        , { gamepadIndex = 2
          , position = vec2 65 60
          , size = 30
          }
        , { gamepadIndex = 3
          , position = vec2 10 50
          , size = 40
          }
        , { gamepadIndex = 4
          , position = vec2 20 80
          , size = 15
          }
        ]
    }



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnAnimationFrame { gamepads, timestamp, dt } ->
            let
                -- Cap dt to 100 milliseconds to avoid time integration problems
                dtCapped =
                    min dt 100
            in
            { model | players = updatePlayersState dtCapped gamepads model.players }



-- view


view : Model -> Html msg
view model =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ div [] [ text "Hold A to grow" ]
        , div [] [ text "Press B once to shrink" ]
        , div [] [ text "(Left) Stick to move around" ]
        , model.players
            |> List.map viewPlayer
            |> div
                [ style "position" "relative"
                , style "background-color" "grey"
                , style "height" "70vh"
                , style "width" "70vh"
                ]
        ]


viewPlayer : Player -> Html msg
viewPlayer player =
    div
        [ style "position" "absolute"
        , style "top" (String.fromFloat (100 - Vec2.getY player.position) ++ "%")
        , style "left" (String.fromFloat (Vec2.getX player.position) ++ "%")
        , style "background-color" "black"
        , style "text" "white"
        , style "width" (String.fromFloat player.size ++ "%")
        , style "height" (String.fromFloat player.size ++ "%")
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "color" "white"
        ]
        [ text (String.fromInt player.gamepadIndex)
        ]



-- main


main =
    Gamepad.Simple.sandbox
        { onAnimationFrame = OnAnimationFrame
        , onBlob = GamepadPort.onBlob
        , saveToLocalStorage = GamepadPort.saveToLocalStorage
        , controls = controls
        }
        { init = init
        , update = update
        , view = view
        }
