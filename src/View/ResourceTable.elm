module View.ResourceTable exposing (view)

import Html as Unstyled
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events

import Model
import View.Style

view : Model.PlayerInfo Float -> Unstyled.Html a
view { username, ready, resources, trade } =
  let
    action value =
      let
        inputId = "action:" ++ value
      in
      [ Html.input
          [ Attributes.type_ "radio"
          , Attributes.id inputId
          , Attributes.name "action"
          , Attributes.value value
          ]
          []
      , Html.label
          [ Attributes.for inputId ]
          [ Html.text value ]
      ]

    tradeQty qty =
      View.Style.tradeQty
        Html.input
        [ Attributes.type_ "text"
        , Attributes.value qty
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
      View.Style.number Html.td [] [Html.text (String.fromInt value)]
  in
  Html.table
    []
    [ View.Style.thead
        []
        [ Html.tr
            []
            [ View.Style.td [] [ Html.text "Action" ]
            , View.Style.td [] [ Html.text "Amount" ]
            , View.Style.td [] [ Html.text "Per turn" ]
            , View.Style.td [] [ Html.text "Upgrade in" ]
            , View.Style.td [] [ Html.text "Trade" ]
            ]
        ]
    , View.Style.tbody
      []
      [ Html.tr
          []
          [ View.Style.td [] (action "Mine")
          , intCell 0
          , intCell 1
          , intCell 1
          , showTrade "M" "C" trade.crafted
          ]
      , Html.tr
        []
        [ View.Style.td [] (action "Craft")
        , intCell 0
        , intCell 1
        , intCell 1
        , showTrade "C" "M" trade.mined
        ]
      ]
    ]
  |> Html.toUnstyled
