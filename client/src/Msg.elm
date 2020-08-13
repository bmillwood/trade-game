module Msg exposing (..)

import Model
import Resource exposing (Resource)

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Connected
  | Accepted Model.Game
  | Failed String

type GameMsg
  = MakeChoice Model.Choices
  | SetReady Bool
  | ServerUpdate Model.Players

type OkMsg
  = PreGame LoginFormMsg
  | InGame GameMsg

type Error
  = ServerDisconnected
  | ServerProtocolError String
  | DriverProtocolError String

errorToString : Error -> String
errorToString error =
  case error of
    ServerDisconnected -> "Server disconnected"
    ServerProtocolError s -> "Server protocol error: " ++ s
    DriverProtocolError s -> "Driver protocol error: " ++ s

type alias Msg = Result Error OkMsg
