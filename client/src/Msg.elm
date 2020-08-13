module Msg exposing (..)

import Model
import Resource exposing (Resource)

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Accepted Model.Game
  | Failed String

type GameMsg
  = MakeChoice Model.Choices
  | SetReady Bool
  | ServerUpdate Model.Players

type Msg
  = PreGame LoginFormMsg
  | InGame GameMsg
  | ServerDecodeError String
