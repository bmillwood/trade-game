module View.PlayerTable exposing (view)

import Dict exposing (Dict)
import Html.Styled as Html exposing (Html)

import ByResource exposing (ByResource)
import Resource
import Model
import View.Style

type alias Row a =
  { ready : Html a
  , username : Html a
  , resources : ByResource (Model.ResourceInfo (Html a))
  , tradeMS : Html a
  , tradeSM : Html a
  }

viewRow : Row a -> Html a
viewRow { ready, username, resources, tradeMC, tradeCM } =
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
      [ View.Style.td [] [ tradeMC ]
      , View.Style.td [] [ tradeCM ]
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
    showTrade give =
      case ByResource.get give trade of
        Nothing -> Html.text "-"
        Just { giveMax, getForEachGive } ->
          Html.text (String.fromFloat getForEachGive ++ " (" ++ String.fromFloat giveMax ++ ")")
  in
  { username = Html.text username
  , ready = if ready then Html.text "✓" else Html.text ""
  , resources = ByResource.map resource resources
  , tradeMS = showTrade Resource.Smelted
  , tradeSM = showTrade Resource.Mined
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
            , tradeMC = Html.text "M/S"
            , tradeCM = Html.text "S/M"
            }
        ]
    , View.Style.tbody
        []
        (List.map (\p -> viewRow (rowFor p)) players)
    ]
