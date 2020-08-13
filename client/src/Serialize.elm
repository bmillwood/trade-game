module Serialize exposing (..)

import Json.Decode
import Json.Encode

import Model
import Msg exposing (Msg)

login : Model.LoginForm -> Json.Encode.Value
login { endpoint, username } =
  Json.Encode.object
    [ ("endpoint", Json.Encode.string endpoint)
    , ("username", Json.Encode.string username)
    ]

unimplemented : Json.Decode.Value -> Msg
unimplemented value =
  Msg.ServerDecodeError "don't know how to read server responses yet"
