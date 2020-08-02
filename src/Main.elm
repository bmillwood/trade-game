module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events

import View.PlayerTable
import View.ResourceTable

type Model = Model

type Msg = Msg

init : () -> (Model, Cmd Msg)
init () =
  ( Model
  , Cmd.none
  )

view : Model -> Html Msg
view Model =
  Html.form
    []
    [ View.ResourceTable.view
    , Html.p [] [Html.button [] [ Html.text "Ready" ]]
    , View.PlayerTable.view
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
