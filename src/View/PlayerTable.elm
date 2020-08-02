module View.PlayerTable exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events

view : Html a
view =
  let
    doIndicCell amDoing contents =
      Html.td (if amDoing then [ Html.Attributes.class "doing" ] else []) contents
  in
  Html.table
    []
    [ Html.thead
        []
        [ Html.tr
            []
            [ Html.td [] [ Html.text "R?" ]
            , Html.td [] [ Html.text "Name" ]
            , Html.td [] [ Html.text "M" ]
            , Html.td [] [ Html.text "M/t" ]
            , Html.td [] [ Html.text "U/M" ]
            , Html.td [] [ Html.text "C" ]
            , Html.td [] [ Html.text "C/t" ]
            , Html.td [] [ Html.text "U/C" ]
            , Html.td [] [ Html.text "M/C" ]
            , Html.td [] [ Html.text "C/M" ]
            ]
        ]
    , Html.tbody
        []
        [ Html.tr
            []
            [ Html.td [] [ Html.text "✓" ]
            , Html.td [] [ Html.text "bmillwood" ]
            , doIndicCell True  [ Html.text "0" ]
            , doIndicCell True  [ Html.text "1" ]
            , doIndicCell True  [ Html.text "1" ]
            , doIndicCell False [ Html.text "0" ]
            , doIndicCell False [ Html.text "1" ]
            , doIndicCell False [ Html.text "1" ]
            , Html.td [] [ Html.text "-" ]
            , Html.td [] [ Html.text "-" ]
            ]
        ]
    ]
