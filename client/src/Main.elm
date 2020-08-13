module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Html exposing (Html)
import Task

import ByResource
import Model exposing (Model)
import Msg exposing (Msg)
import Ports
import Resource
import View

init : () -> (Model, Cmd Msg)
init () =
  ( Model.PreGame
      { loginState = Model.NotSubmitted
      , loginForm = { endpoint = "ws://localhost:45286", username = "" }
      }
  , Cmd.none
  )

view : Model -> Html Msg
view = View.view

fakeNewGame : { username : String } -> Model.Game
fakeNewGame { username } =
  { choices =
      { action = Nothing
      , trade = ByResource.both { giveMax = "-", getForEachGive = "-" }
      }
  , players =
      { me =
          { username = username
          , ready = False
          , resources = ByResource.both { held = 0, increment = 1, upgradeIn = 1 }
          , trade = ByResource.both Nothing
          }
      , others = []
      }
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    ignore = (model, Cmd.none)
  in
  case model of
    Model.PreGame preGame ->
      let
        failed error =
          ( Model.PreGame { preGame | loginState = Model.Failed error }
          , Cmd.none
          )
      in
      case msg of
        Err error -> failed (Msg.errorToString error)
        Ok (Msg.InGame _) -> ignore
        Ok (Msg.PreGame pMsg) ->
          case pMsg of
            Msg.Update newForm ->
              ( Model.PreGame { preGame | loginForm = newForm }, Cmd.none )
            Msg.Submit ->
              ( Model.PreGame { preGame | loginState = Model.Waiting }
              , Ports.connect { endpoint = preGame.loginForm.endpoint }
              )
            Msg.Connected ->
              ( model
              , Ports.login { username = preGame.loginForm.username }
              )
            Msg.Accepted players ->
              ( Model.InGame { choices = Model.newChoices, players = players }, Cmd.none )
            Msg.Failed error ->
              failed error
    Model.InGame game ->
      case msg of
        Err _ -> ignore
        Ok (Msg.PreGame _) -> ignore
        Ok (Msg.InGame gameMsg) ->
          case gameMsg of
            Msg.MakeChoice choices ->
              (Model.InGame { game | choices = choices }, Cmd.none)
            Msg.SetReady isReady ->
              let
                oldPlayers = game.players
                oldMe = oldPlayers.me
                newPlayers = { oldPlayers | me = { oldMe | ready = isReady } }
              in
              -- also need to send choices to the server
              (Model.InGame { game | players = newPlayers }, Cmd.none)
            Msg.ServerUpdate newPlayers ->
              (Model.InGame { game | players = newPlayers }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Ports.subscriptions model

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
