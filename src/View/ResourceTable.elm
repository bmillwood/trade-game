module View.ResourceTable exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events

import Model

view : Model.PlayerInfo Float -> Html a
view { username, ready, resources, trade } =
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

    showTradeParams params =
      case params of
        Nothing -> { giveMax = "-", getForEachGive = "-" }
        Just { giveMax, getForEachGive } ->
          { giveMax = String.fromFloat giveMax
          , getForEachGive = String.fromFloat getForEachGive
          }

    showTrade give get params =
      case showTradeParams params of
        { giveMax, getForEachGive } ->
          Html.td
            []
            [ tradeQty giveMax
            , Html.text give
            , Html.text " for "
            , tradeQty getForEachGive
            , Html.text (get ++ " each")
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
          , showTrade "M" "C" trade.crafted
          ]
      , Html.tr
        []
        [ Html.td [] (action "Craft")
        , intCell 0
        , intCell 1
        , intCell 1
        , showTrade "C" "M" trade.mined
        ]
      ]
    ]
