module View exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events

import Model
import Msg exposing (Msg)
import View.PlayerTable
import View.ResourceTable

viewPregame : Model.LoginForm -> Html Msg.LoginFormMsg
viewPregame { username } =
  Html.form
    [ Html.Events.onSubmit Msg.Submit ]
    [ Html.text "Username: "
    , Html.input
        [ Html.Attributes.type_ "text"
        , Html.Events.onInput (\newName -> Msg.Update { username = newName })
        ]
        [ Html.text username ]
    ]

viewGame : Model.Game -> Html Msg.GameMsg
viewGame { choices, players } =
  Html.div
    []
    [ View.ResourceTable.view choices players.me
    , Html.p
        []
        [ Html.button
            [ Html.Events.onClick (Msg.SetReady True)
            , Html.Attributes.disabled players.me.ready
            ]
            [ Html.text "Ready" ]
        ]
    , View.PlayerTable.view (players.me :: players.others)
    ]

view : Model.Model -> Html Msg
view model =
  case model of
    Model.PreGame loginForm ->
      Html.map Msg.PreGame (viewPregame loginForm)
    Model.InGame game ->
      Html.map Msg.InGame (viewGame game)
