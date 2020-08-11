module View exposing (view)

import Html as Unstyled
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events

import Model
import Msg exposing (Msg)
import View.PlayerTable
import View.ResourceTable
import View.Style

viewPregame : Model.LoginForm -> Html Msg.LoginFormMsg
viewPregame { username } =
  Html.form
    [ Events.onSubmit Msg.Submit ]
    [ Html.text "Username: "
    , Html.input
        [ Attributes.type_ "text"
        , Events.onInput (\newName -> Msg.Update { username = newName })
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
            [ Events.onClick (Msg.SetReady True)
            , Attributes.disabled players.me.ready
            ]
            [ Html.text "Ready" ]
        ]
    , View.PlayerTable.view (players.me :: players.others)
    ]

viewStyled : Model.Model -> Html Msg
viewStyled model =
  case model of
    Model.PreGame loginForm ->
      Html.map Msg.PreGame (viewPregame loginForm)
    Model.InGame game ->
      Html.map Msg.InGame (viewGame game)

view : Model.Model -> Unstyled.Html Msg
view = Html.toUnstyled << viewStyled
