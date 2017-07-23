
{-
    API: Gamepad.Remap
    ------------------

    type alias Model

    type Msg

    type Outcome
      = StillOpen Model
      | Done Config

    view : Model -> Html Msg

    update : Config -> Msg -> Model -> (Outcome, Cmd.msg)

    subscriptions : (TimeAndGamepads -> msg) -> Model -> Sub msg


    -- the remap utility will not ask the user for inputs that are Nothing
    type alias ControlNames =
      { leftStick : Just String
      , rightStick : Just String
      , dPad : Just String
      , leftShoulder : Just String
      , ...
      }

    baseControlNames : ControlNames
    baseControlNames =
      { leftStick = Nothing
      , rightStick = Nothing
      , ...
      }

-}
