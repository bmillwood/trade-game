module View.ResourceTable exposing (view)

import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events

import ByDir exposing (ByDir)
import ByResource
import Model
import Msg
import Resource exposing (Resource)
import Trade
import View.Style

type alias Row a =
  { action : Html a
  , resourceInfo : Model.ResourceInfo (Html a)
  , trade : ByDir (Html a)
  }

viewRow : Row a -> Html a
viewRow { action, resourceInfo, trade } =
  Html.tr
    []
    [ action
    , resourceInfo.held
    , resourceInfo.increment
    , resourceInfo.upgradeIn
    , trade.buy
    , trade.sell
    ]

rowFor : Model.Choices -> Model.PlayerInfo -> Resource -> Row Model.Choices
rowFor choices me resource =
  let
    resources = ByResource.get resource me.resources

    tradeQty placeholder qty onInput =
      View.Style.tradeQty
        Html.input
        [ Attributes.type_ "text"
        , Attributes.value qty
        , Attributes.disabled me.ready
        , Attributes.placeholder placeholder
        , Events.onInput onInput
        ]
        []

    tradeCell dir =
      case resource of
        Resource.Smelted -> View.Style.td [] [ Html.text "-" ]
        Resource.Mined ->
          let
            tradeMined = ByDir.get dir choices.tradeMined
            setTrade newTrade = { choices | tradeMined = ByDir.set dir newTrade choices.tradeMined }
          in
          View.Style.td
            []
            [ tradeQty
                "qty"
                tradeMined.size
                (\newSize -> setTrade { tradeMined | size = newSize })
            , Html.text " for "
            , tradeQty
                "px"
                tradeMined.price
                (\newPrice -> setTrade { tradeMined | price = newPrice })
            , Html.text "S each"
            ]

    floatCell value =
      View.Style.number View.Style.td [] [Html.text (String.fromFloat value)]

    actionSelectCell =
      let
        actionText =
          case resource of
            Resource.Mined -> "Mine"
            Resource.Smelted -> "Smelt"
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
  in
  { action = actionSelectCell
  , resourceInfo =
      { held = floatCell resources.held
      , increment = floatCell resources.increment
      , upgradeIn = floatCell resources.upgradeIn
      }
  , trade = ByDir.create tradeCell
  }

headerRow : Row a
headerRow =
  let
    textTd text = View.Style.td [] [ Html.text text ]
  in
  { action = textTd "Action"
  , resourceInfo =
      { held = textTd "Amount"
      , increment = textTd "Per turn"
      , upgradeIn = textTd "Upgrade in"
      }
  , trade = { buy = textTd "Buy", sell = textTd "Sell" }
  }

view : Model.Choices -> Model.PlayerInfo -> Html Model.Choices
view choices me =
  Html.table
    []
    [ View.Style.thead
        []
        [ viewRow headerRow
        ]
    , View.Style.tbody
      []
      [ viewRow (rowFor choices me Resource.Mined)
      , viewRow (rowFor choices me Resource.Smelted)
      ]
    ]
