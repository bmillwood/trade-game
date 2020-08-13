module Serialize exposing (..)

import Json.Decode
import Json.Encode

import Model

login : Model.LoginForm -> Json.Encode.Value
login { endpoint, username } =
  Json.Encode.object
    [ ("endpoint", Json.Encode.string endpoint)
    , ("username", Json.Encode.string username)
    ]
