module Msg exposing (..)

import Model
import Resource exposing (Resource)

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Accepted Model.Game

type GameMsg
  = MakeChoice Model.Choices
  | SetReady Bool
  | ServerUpdate Model.Players

type Msg
  = PreGame LoginFormMsg
  | InGame GameMsg
