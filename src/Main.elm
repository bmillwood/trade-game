module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Task

import ByResource
import Model exposing (Model)
import Msg exposing (Msg)
import Resource
import View

initResourceInfo : Model.ResourceInfo Float
initResourceInfo = { held = 0, increment = 1, upgradeIn = 1 }

init : () -> (Model, Cmd Msg)
init () =
  ( Model.PreGame { username = "" }
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

submitLogin : Model.LoginForm -> Cmd Msg
submitLogin username =
  Task.perform identity (Task.succeed (Msg.PreGame (Msg.Accepted (fakeNewGame username))))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (Msg.PreGame formMsg, Model.PreGame loginForm) ->
      case formMsg of
        Msg.Update newForm -> (Model.PreGame newForm, Cmd.none)
        Msg.Submit -> (model, submitLogin loginForm)
        Msg.Accepted game -> (Model.InGame game, Cmd.none)
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
subscriptions _ = Sub.none

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
