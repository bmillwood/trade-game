module View.PlayerTable exposing (view)

import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)

import ByDir exposing (ByDir)
import ByResource exposing (ByResource)
import Resource
import Model
import Trade
import View.Style

type alias Row a =
  { ready : Html a
  , username : Html a
  , resources : ByResource (Model.ResourceInfo (Html a))
  , trade : ByDir (Html a)
  }

viewRow : Row a -> Html a
viewRow { ready, username, resources, trade } =
  let
    beforeResources =
      [ View.Style.td [] [ ready ]
      , View.Style.td [] [ username ]
      ]
    inResource { held, increment, upgradeIn } =
      [ View.Style.td [] [ held ]
      , View.Style.td [] [ increment ]
      , View.Style.td [] [ upgradeIn ]
      ]
    inResources =
      case resources of
        { mined, smelted } -> inResource mined ++ inResource smelted
    afterResources =
      [ View.Style.td [] [ trade.buy ]
      , View.Style.td [] [ trade.sell ]
      ]
  in
  Html.tr
    []
    (beforeResources ++ inResources ++ afterResources)

rowFor : Model.PlayerInfo -> Row a
rowFor { username, ready, resources, trade } =
  let
    qtyText n = Html.text (String.fromFloat n)
    resource { held, increment, upgradeIn } =
      { held = qtyText held
      , increment = qtyText increment
      , upgradeIn = qtyText upgradeIn
      }
    showTrade maybeTrade =
      case maybeTrade of
        Nothing -> Html.text ""
        Just { price, size } ->
          Html.text (String.fromFloat size ++ " @ " ++ String.fromFloat price ++ "S")
  in
  { username = Html.text username
  , ready = if ready then Html.text "✓" else Html.text ""
  , resources = ByResource.map resource resources
  , trade = ByDir.map showTrade trade
  }

view : List Model.PlayerInfo -> Html a
view players =
  Html.table
    []
    [ View.Style.thead
        []
        [ viewRow
            { ready = Html.text "R?"
            , username = Html.text "Name"
            , resources =
                { mined =
                    { held = Html.text "M"
                    , increment = Html.text "M/t"
                    , upgradeIn = Html.text "U/M"
                    }
                , smelted =
                    { held = Html.text "S"
                    , increment = Html.text "S/t"
                    , upgradeIn = Html.text "U/S"
                    }
                }
            , trade =
                { buy = Html.text "Buy M"
                , sell = Html.text "Sell M"
                }
            }
        ]
    , View.Style.tbody
        []
        (List.map (\p -> viewRow (rowFor p)) players)
    ]
