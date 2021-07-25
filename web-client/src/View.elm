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

viewPreGame : Model.PreGameState -> Html Msg.LoginFormMsg
viewPreGame { loginState, loginForm } =
  let
    endpointInput =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.name "endpoint"
        , Attributes.value loginForm.endpoint
        , Events.onInput (\input -> Msg.Update { loginForm | endpoint = input })
        , Attributes.disabled (loginState == Model.Waiting)
        ]
        []

    usernameInput =
      Html.input
        [ Attributes.type_ "text"
        , Attributes.name "username"
        , Attributes.value loginForm.username
        , Events.onInput (\input -> Msg.Update { loginForm | username = input })
        , Attributes.disabled (loginState == Model.Waiting)
        ]
        []

    spectateInput =
      Html.input
        [ Attributes.type_ "checkbox"
        , Attributes.name "spectate"
        , Attributes.checked (loginForm.kind == Model.Spectator)
        , Events.onCheck (\checked ->
            Msg.Update { loginForm | kind = if checked then Model.Spectator else Model.Player })
        , Attributes.disabled (loginState == Model.Waiting)
        ]
        []

    submitButton =
      Html.input
        [ Attributes.type_ "submit"
        , Attributes.name "login"
        , Attributes.value "Login"
        ]
        []
  in
  Html.form
    [ Events.onSubmit Msg.Submit ]
    [ Html.p [] [ Html.text "Server: ", endpointInput ]
    , Html.p [] [ Html.text "Username: ", usernameInput ]
    , Html.p [] [ Html.text "Spectate: ", spectateInput ]
    , Html.p [] [ submitButton]
    ]

viewControls : Model.Choices -> Model.PlayerInfo -> List (Html Msg.GameMsg)
viewControls choices me =
  [ View.ResourceTable.view choices me
    |> Html.map Msg.MakeChoice
  , Html.p
      []
      [ Html.button
          [ Events.onClick (Msg.SetMeReady True)
          , Attributes.disabled me.ready
          ]
          [ Html.text "Ready" ]
      ]
  ]

viewPlayers : List Model.PlayerInfo -> List (Html Msg.GameMsg)
viewPlayers players =
  [ View.PlayerTable.view players
    |> Html.map never
  ]

viewGame : Model.Game -> Html Msg.GameMsg
viewGame { choices, players } =
  Html.div
    []
    (case players.me of
      Nothing -> viewPlayers players.others
      Just me ->
        viewControls choices me ++ viewPlayers players.others)

viewStyled : Model.Model -> Html Msg
viewStyled { error, state } =
  let
    errorDisplay =
      case error of
        Nothing ->
          []
        Just errorMsg ->
          [ View.Style.errorDisplay [] [ Html.text errorMsg ] ]

    stateDisplay =
      [ case state of
          Model.PreGame preGame ->
            Html.map (Ok << Msg.PreGame) (viewPreGame preGame)
          Model.InGame game ->
            Html.map (Ok << Msg.InGame) (viewGame game)
      ]
  in
  Html.div [] (errorDisplay ++ stateDisplay)

view : Model.Model -> Unstyled.Html Msg
view = Html.toUnstyled << viewStyled
