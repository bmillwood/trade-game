module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events

import Model exposing (Model)
import View.PlayerTable
import View.ResourceTable

type Msg = Msg

initResourceInfo : Model.ResourceInfo Float
initResourceInfo = { held = 0, increment = 1, upgradeIn = 1 }

initMe : Model.PlayerInfo Float
initMe =
  { username = "bmillwood"
  , ready = True
  , resources = { mined = initResourceInfo, crafted = initResourceInfo }
  , trade = { mined = Nothing, crafted = Nothing }
  }

init : () -> (Model, Cmd Msg)
init () =
  ( { me = initMe, others = [] }
  , Cmd.none
  )

view : Model -> Html Msg
view { me, others } =
  Html.form
    []
    [ View.ResourceTable.view me
    , Html.p [] [Html.button [] [ Html.text "Ready" ]]
    , View.PlayerTable.view (me :: others)
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
      newModel = model
  in
  (newModel, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
