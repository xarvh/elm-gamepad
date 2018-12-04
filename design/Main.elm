module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


inputs =
    List.repeat 20 "i"


actions =
    List.repeat 10 "a"


main =
    inputs
        |> List.map viewInput
        |> div []


viewInput inp =
    div
        [ style "display" "flex"
        , style
        ]
        [ div [] [ text "Button 13" ]
        , div [] [ text "[-----|--]  [x]" ]
        , select
            []
            [ option [] [ text "Alternate Fire" ]
            ]
        , select
            []
            [ option [] [ text "Unused" ]
            ]
        ]


viewAction action =
    div [] [ text "Alternate Fire" ]
