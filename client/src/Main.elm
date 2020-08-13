port module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Json.Decode
import Json.Encode
import Html exposing (Html)
import Task

import ByResource
import Model exposing (Model)
import Msg exposing (Msg)
import Resource
import Serialize
import View

port sendLogin : Json.Encode.Value -> Cmd msg
port receiveFromServer : (Json.Decode.Value -> msg) -> Sub msg

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
  case (msg, model) of
    (Msg.PreGame formMsg, Model.PreGame preGame) ->
      case formMsg of
        Msg.Update newForm -> (Model.PreGame { preGame | loginForm = newForm }, Cmd.none)
        Msg.Submit ->
          ( Model.PreGame { preGame | loginState = Model.Waiting }
          , sendLogin (Serialize.login preGame.loginForm)
          )
        Msg.Accepted game -> (Model.InGame game, Cmd.none)
        Msg.Failed error -> (Model.PreGame { preGame | loginState = Model.Failed error }, Cmd.none)
    (Msg.ServerDecodeError error, Model.PreGame preGame) ->
      (Model.PreGame { preGame | loginState = Model.Failed error }, Cmd.none)
    (Msg.InGame gameMsg, Model.InGame game) ->
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
    (_, _) ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  receiveFromServer Serialize.unimplemented

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
