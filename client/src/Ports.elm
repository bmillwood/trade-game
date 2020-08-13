port module Ports exposing (..)

import Json.Decode
import Json.Encode

import Model
import Msg exposing (Msg)

port sendToJS : Json.Encode.Value -> Cmd msg
port receiveFromJS : (Json.Decode.Value -> msg) -> Sub msg

request : { kind : String, payload : Json.Encode.Value } -> Cmd msg
request { kind, payload } =
  Json.Encode.object
    [ ("kind", Json.Encode.string kind)
    , ("payload", payload)
    ]
  |> sendToJS

connect : { endpoint : String } -> Cmd msg
connect { endpoint } = request { kind = "connect", payload = Json.Encode.string endpoint }

send : Json.Encode.Value -> Cmd msg
send value = request { kind = "send", payload = value }

login : { username : String } -> Cmd msg
login { username } =
  Json.Encode.object [ ("username", Json.Encode.string username) ]
  |> send

type ConnectionStatus
  = Connected
  | Disconnected

type FromJS
  = ServerStatus ConnectionStatus
  | FromServer String

connectionStatus : Json.Decode.Decoder ConnectionStatus
connectionStatus =
  Json.Decode.string
  |> Json.Decode.andThen (\v ->
       case v of
         "connected" -> Json.Decode.succeed Connected
         "disconnected" -> Json.Decode.succeed Disconnected
         other -> Json.Decode.fail ("don't know how to interpret server status: " ++ other))

fromJS : Json.Decode.Decoder FromJS
fromJS =
  let
    payload decoder = Json.Decode.field "payload" decoder
  in
  Json.Decode.field "kind" Json.Decode.string
  |> Json.Decode.andThen (\kindString ->
       case kindString of
         "server-status" -> Json.Decode.map ServerStatus (payload connectionStatus)
         "from-server" -> Json.Decode.map FromServer (payload Json.Decode.string)
         other -> Json.Decode.fail ("unknown message kind: " ++ other))

preGame : Model.PreGameState -> FromJS -> Msg
preGame model msg =
  case msg of
    ServerStatus Connected -> Ok (Msg.PreGame Msg.Connected)
    ServerStatus Disconnected -> Err Msg.ServerDisconnected
    FromServer s -> Err (Msg.ServerProtocolError "unhandled FromServer")

subscriptions : Model.Model -> Sub Msg
subscriptions model =
  let
    handler =
      case model of
        Model.PreGame preGameState -> preGame preGameState
        Model.InGame game -> (\msg -> Err (Msg.ServerProtocolError "we don't handle InGame messages yet"))
  in
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue (Json.Decode.map handler fromJS) value of
      Ok msg -> msg
      Err error -> Err (Msg.DriverProtocolError (Json.Decode.errorToString error))
