module Main exposing (main)

import Browser
import Json.Decode
import Html exposing (Html)
import Task

import ByResource
import LocationParser
import Model exposing (Model)
import Msg exposing (Msg)
import Ports
import Resource
import View

init : Json.Decode.Value -> (Model, Cmd Msg)
init flags =
  let
    { error, endpoint, username, autoLogin, spectate } = LocationParser.parseLocation flags
  in
  ( { error = error
    , state =
        Model.PreGame
          { loginState = Model.NotSubmitted
          , loginForm = { endpoint = endpoint, username = username, kind = if spectate then Model.Spectator else Model.Player }
          }
    }
  , if autoLogin
    then Task.perform identity (Task.succeed (Ok (Msg.PreGame Msg.Submit)))
    else Cmd.none
  )

view : Model -> Html Msg
view = View.view

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    ignore = (model, Cmd.none)
  in
  case model.state of
    Model.PreGame preGame ->
      let
        updatePreGame newPreGame = { model | state = Model.PreGame newPreGame }

        failed error =
          ( { model
            | state = Model.PreGame { preGame | loginState = Model.Failed }
            , error = Just error
            }
          , Cmd.none
          )
      in
      case msg of
        Err error -> failed (Msg.errorToString error)
        Ok (Msg.InGame _) -> ignore
        Ok (Msg.PreGame pMsg) ->
          case pMsg of
            Msg.Update newForm ->
              ( updatePreGame { preGame | loginForm = newForm }, Cmd.none )
            Msg.Submit ->
              ( updatePreGame { preGame | loginState = Model.Waiting }
              , Ports.connect { endpoint = preGame.loginForm.endpoint }
              )
            Msg.Connected ->
              ( model
              , Ports.login
                  { username = preGame.loginForm.username
                  , kind = preGame.loginForm.kind
                  }
              )
            Msg.Accepted players ->
              ( { model | state = Model.InGame { choices = Model.newChoices, players = players } }
              , Cmd.none
              )
            Msg.Failed error ->
              failed error
    Model.InGame game ->
      let
        updateGame newGame = { model | state = Model.InGame newGame }
      in
      case msg of
        Err error -> ( { model | error = Just (Msg.errorToString error) }, Cmd.none )
        Ok (Msg.PreGame _) -> ignore
        Ok (Msg.InGame gameMsg) ->
          case gameMsg of
            Msg.MakeChoice choices ->
              ( updateGame { game | choices = choices }, Cmd.none )
            Msg.SetMeReady isReady ->
              case game.players.me of
                Nothing -> ignore
                Just oldMe ->
                  let
                    oldPlayers = game.players
                    newPlayers = { oldPlayers | me = Just { oldMe | ready = isReady } }
                  in
                  ( updateGame { game | players = newPlayers }
                  , Ports.sendChoices game.choices
                  )
            Msg.SetOtherReady username isReady ->
              let
                oldPlayers = game.players
                oldOthers = oldPlayers.others
                updateReady other = if other.username == username then { other | ready = isReady } else other
                newPlayers = { oldPlayers | others = List.map updateReady oldOthers }
              in
              ( updateGame { game | players = newPlayers }, Cmd.none )
            Msg.ServerUpdate newPlayers ->
              ( updateGame { game | players = newPlayers }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model = Ports.subscriptions model

main =
  Browser.element
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }
