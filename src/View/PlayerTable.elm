module View.PlayerTable exposing (view)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events

import ByResource exposing (ByResource)
import Resource
import Model

type alias Row a =
  { ready : Html a
  , username : Html a
  , resources : ByResource (Model.ResourceInfo (Html a))
  , tradeMC : Html a
  , tradeCM : Html a
  }

viewRow : Row a -> Html a
viewRow { ready, username, resources, tradeMC, tradeCM } =
  let
    beforeResources =
      [ Html.td [] [ ready ]
      , Html.td [] [ username ]
      ]
    inResource { held, increment, upgradeIn } =
      [ Html.td [] [ held ]
      , Html.td [] [ increment ]
      , Html.td [] [ upgradeIn ]
      ]
    inResources =
      case resources of
        { mined, crafted } -> inResource mined ++ inResource crafted
    afterResources =
      [ Html.td [] [ tradeMC ]
      , Html.td [] [ tradeCM ]
      ]
  in
  Html.tr
    []
    (beforeResources ++ inResources ++ afterResources)

rowFor : Model.PlayerInfo Float -> Row a
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
  , tradeMC = showTrade Resource.Crafted
  , tradeCM = showTrade Resource.Mined
  }

view : List (Model.PlayerInfo Float) -> Html a
view players =
  Html.table
    []
    [ Html.thead
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
                , crafted =
                    { held = Html.text "C"
                    , increment = Html.text "C/t"
                    , upgradeIn = Html.text "U/C"
                    }
                }
            , tradeMC = Html.text "M/C"
            , tradeCM = Html.text "C/M"
            }
        ]
    , Html.tbody
        []
        (List.map (\p -> viewRow (rowFor p)) players)
    ]
