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
viewPregame loginForm =
  Html.form
    [ Events.onSubmit Msg.Submit ]
    [ Html.p
        []
        [ Html.text "Server: "
        , Html.input
            [ Attributes.type_ "text"
            , Attributes.name "endpoint"
            , Attributes.value loginForm.endpoint
            , Events.onInput (\input -> Msg.Update { loginForm | endpoint = input })
            ]
            []
        ]
    , Html.p
        []
        [ Html.text "Username: "
        , Html.input
            [ Attributes.type_ "text"
            , Attributes.name "username"
            , Attributes.value loginForm.username
            , Events.onInput (\input -> Msg.Update { loginForm | username = input })
            ]
            []
        ]
    , Html.p
        []
        [ Html.input
            [ Attributes.type_ "submit"
            , Attributes.name "login"
            , Attributes.value "Login"
            ]
            []
        ]
    ]

viewGame : Model.Game -> Html Msg.GameMsg
viewGame { choices, players } =
  Html.div
    []
    [ View.ResourceTable.view choices players.me
      |> Html.map Msg.MakeChoice
    , Html.p
        []
        [ Html.button
            [ Events.onClick (Msg.SetReady True)
            , Attributes.disabled players.me.ready
            ]
            [ Html.text "Ready" ]
        ]
    , View.PlayerTable.view (players.me :: players.others)
      |> Html.map never
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
