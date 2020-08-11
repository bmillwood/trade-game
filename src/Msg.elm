module Msg exposing (..)

import Model

type LoginFormMsg
  = Update Model.LoginForm
  | Submit
  | Accepted Model.Game

type alias GameMsg
  = Never

type Msg
  = PreGame LoginFormMsg
  | InGame GameMsg
