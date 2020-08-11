module View.ResourceTable exposing (view)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events

import ByResource
import Model
import Resource
import View.Style

view : Model.Choices -> Model.PlayerInfo -> Html a
view { action, trade } { username, ready, resources } =
  let
    tradeQty qty =
      View.Style.tradeQty
        Html.input
        [ Attributes.type_ "text"
        , Attributes.value qty
        , Attributes.disabled ready
        ]
        []

    showTradeParams params =
      case params of
        Nothing -> { giveMax = "-", getForEachGive = "-" }
        Just { giveMax, getForEachGive } ->
          { giveMax = String.fromFloat giveMax
          , getForEachGive = String.fromFloat getForEachGive
          }

    showTrade { give, get } { giveMax, getForEachGive } =
      let
        tradeMaybeQty qty =
          Maybe.map String.fromFloat qty
          |> Maybe.withDefault "-"
          |> tradeQty
      in
      Html.td
        []
        [ Html.text "Offer up to "
        , tradeMaybeQty giveMax
        , Html.text give
        , Html.text " for "
        , tradeMaybeQty getForEachGive
        , Html.text (get ++ " each")
        ]

    floatCell value =
      View.Style.number Html.td [] [Html.text (String.fromFloat value)]

    resourceRow resource =
      let
        (actionText, give, get) =
          case resource of
            Resource.Mined -> ("Mine", "C", "M")
            Resource.Crafted -> ("Craft", "M", "C")
        inputId = "action:" ++ actionText
        resourceInfo = ByResource.get resource resources
      in
      Html.tr
        []
        [ View.Style.td
            []
            [ Html.input
                [ Attributes.type_ "radio"
                , Attributes.id inputId
                , Attributes.name "action"
                , Attributes.value actionText
                , Attributes.disabled ready
                ]
                []
            , Html.label
              [ Attributes.for inputId ]
              [ Html.text actionText ]
            ]
        , floatCell resourceInfo.held
        , floatCell resourceInfo.increment
        , floatCell resourceInfo.upgradeIn
        , showTrade { give = give, get = get } (ByResource.get resource trade)
        ]
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
      [ resourceRow Resource.Mined
      , resourceRow Resource.Crafted
      ]
    ]
