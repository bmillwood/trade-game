module View.ResourceTable exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events

view : Html a
view =
  let
    action value =
      let
        inputId = "action:" ++ value
      in
      [ Html.input
          [ Html.Attributes.type_ "radio"
          , Html.Attributes.id inputId
          , Html.Attributes.name "action"
          , Html.Attributes.value value
          ]
          []
      , Html.label
          [ Html.Attributes.for inputId ]
          [ Html.text value ]
      ]

    tradeQty qty =
      Html.input
        [ Html.Attributes.type_ "text"
        , Html.Attributes.class "tradeQty"
        , Html.Attributes.value qty
        ]
        []

    trade give get =
      Html.td
        []
        [ tradeQty "-"
        , Html.text give
        , Html.text " for "
        , tradeQty "-"
        , Html.text get
        ]

    intCell value =
      Html.td [Html.Attributes.class "number"] [Html.text (String.fromInt value)]
  in
  Html.table
    []
    [ Html.thead
        []
        [ Html.tr
            []
            [ Html.td [] [ Html.text "Action" ]
            , Html.td [] [ Html.text "Amount" ]
            , Html.td [] [ Html.text "Per turn" ]
            , Html.td [] [ Html.text "Upgrade in" ]
            , Html.td [] [ Html.text "Trade" ]
            ]
        ]
    , Html.tbody
      []
      [ Html.tr
          []
          [ Html.td [] (action "Mine")
          , intCell 0
          , intCell 1
          , intCell 1
          , trade "M" "C"
          ]
      , Html.tr
        []
        [ Html.td [] (action "Craft")
        , intCell 0
        , intCell 1
        , intCell 1
        , trade "C" "M"
        ]
      ]
    ]
