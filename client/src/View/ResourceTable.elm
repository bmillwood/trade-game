module View.ResourceTable exposing (view)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events

import ByResource
import Model
import Msg
import Resource
import View.Style

view : Model.Choices -> Model.PlayerInfo -> Html Model.Choices
view choices me =
  let
    tradeQty qty onInput =
      View.Style.tradeQty
        Html.input
        [ Attributes.type_ "text"
        , Attributes.value qty
        , Attributes.disabled me.ready
        , Events.onInput onInput
        ]
        []

    tradeCell resource =
      let
        (give, get) =
          case resource of
            Resource.Mined -> ("C", "M")
            Resource.Crafted -> ("M", "C")
        params = ByResource.get resource choices.trade
        setParams newParams = { choices | trade = ByResource.set resource newParams choices.trade }
      in
      Html.td
        []
        [ Html.text "Offer up to "
        , tradeQty params.giveMax (\inp -> setParams { params | giveMax = inp })
        , Html.text give
        , Html.text " for "
        , tradeQty params.getForEachGive (\inp -> setParams { params | getForEachGive = inp })
        , Html.text (get ++ " each")
        ]

    floatCell value =
      View.Style.number Html.td [] [Html.text (String.fromFloat value)]

    actionSelectCell resource =
      let
        actionText =
          case resource of
            Resource.Mined -> "Mine"
            Resource.Crafted -> "Craft"
        inputId = "action:" ++ actionText
      in
      View.Style.td
            []
            [ Html.input
                [ Attributes.type_ "radio"
                , Attributes.id inputId
                , Attributes.name "action"
                , Attributes.value actionText
                , Attributes.disabled me.ready
                , Events.onClick { choices | action = Just resource }
                ]
                []
            , Html.label
              [ Attributes.for inputId ]
              [ Html.text actionText ]
            ]

    resourceRow resource =
      let
        resourceInfo = ByResource.get resource me.resources
      in
      Html.tr
        []
        [ actionSelectCell resource
        , floatCell resourceInfo.held
        , floatCell resourceInfo.increment
        , floatCell resourceInfo.upgradeIn
        , tradeCell resource
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
