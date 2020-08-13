port module Ports exposing (..)

import Dict
import Json.Decode
import Json.Encode

import ByResource exposing (ByResource)
import Model
import Msg exposing (Msg)
import Resource exposing (Resource)

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

plainVariant : { name : String } -> List (String, a) -> Json.Decode.Decoder a
plainVariant { name } values =
  let
    dict = Dict.fromList values
  in
  Json.Decode.string
  |> Json.Decode.andThen (\k ->
       case Dict.get k dict of
         Just v -> Json.Decode.succeed v
         Nothing -> Json.Decode.fail (name ++ ": unknown variant: " ++ k))

type VariantSpec a
  = Plain a
  | WithContents (Json.Decode.Decoder a)

variant : { name : String } -> List (String, VariantSpec a) -> Json.Decode.Decoder a
variant { name } values =
  let
    dict = Dict.fromList values
  in
  Json.Decode.field "tag" Json.Decode.string
  |> Json.Decode.andThen (\k ->
       case Dict.get k dict of
         Nothing -> Json.Decode.fail (name ++ ": unknown tag: " ++ k)
         Just (Plain v) -> Json.Decode.succeed v
         Just (WithContents decoder) -> Json.Decode.field "contents" decoder)

type ServerMsg
  = LoginAccepted Model.Players

type FromJS
  = ServerStatus ConnectionStatus
  | FromServer ServerMsg

connectionStatus : Json.Decode.Decoder ConnectionStatus
connectionStatus =
  plainVariant
    { name = "connectionStatus" }
    [ ("connected", Connected)
    , ("disconnected", Disconnected)
    ]

byResource : Json.Decode.Decoder a -> Json.Decode.Decoder (ByResource a)
byResource decoder =
  Json.Decode.map2
    (\mined crafted -> { mined = mined, crafted = crafted })
    (Json.Decode.field "mined" decoder)
    (Json.Decode.field "crafted" decoder)

tradeParam : Json.Decode.Decoder a -> Json.Decode.Decoder (Model.TradeParam a)
tradeParam decoder =
  Json.Decode.map2
    (\giveMax getForEachGive -> { giveMax = giveMax, getForEachGive = getForEachGive })
    (Json.Decode.field "giveMax" decoder)
    (Json.Decode.field "getForEachGive" decoder)

resourceInfo : Json.Decode.Decoder (Model.ResourceInfo Float)
resourceInfo =
  Json.Decode.map3
    (\held increment upgradeIn -> { held = held, increment = increment, upgradeIn = upgradeIn })
    (Json.Decode.field "held" Json.Decode.float)
    (Json.Decode.field "increment" Json.Decode.float)
    (Json.Decode.field "upgradeIn" Json.Decode.float)

playerInfo : Json.Decode.Decoder Model.PlayerInfo
playerInfo =
  Json.Decode.map4
    (\username ready resources trade ->
       { username = username
       , ready = ready
       , resources = resources
       , trade = trade
       })
    (Json.Decode.field "username" Json.Decode.string)
    (Json.Decode.field "ready" Json.Decode.bool)
    (Json.Decode.field "resources" (byResource resourceInfo))
    (Json.Decode.field "trade" (byResource (Json.Decode.nullable (tradeParam Json.Decode.float))))

players : Json.Decode.Decoder Model.Players
players =
  Json.Decode.map2
    (\me others -> { me = me, others = others })
    (Json.Decode.field "me" playerInfo)
    (Json.Decode.field "others" (Json.Decode.list playerInfo))

serverMsg : Json.Decode.Decoder ServerMsg
serverMsg =
  variant
    { name = "serverMsg" }
    [ ("LoginAccepted", WithContents (Json.Decode.map LoginAccepted players))
    ]

fromJS : Json.Decode.Decoder FromJS
fromJS =
  variant
    { name = "fromJS" }
    [ ("server-status", WithContents (Json.Decode.map ServerStatus connectionStatus))
    , ("from-server", WithContents (Json.Decode.map FromServer serverMsg))
    ]

preGame : Model.PreGameState -> FromJS -> Msg
preGame model msg =
  case msg of
    ServerStatus Connected -> Ok (Msg.PreGame Msg.Connected)
    ServerStatus Disconnected -> Err Msg.ServerDisconnected
    FromServer (LoginAccepted newPlayers) -> Ok (Msg.PreGame (Msg.Accepted newPlayers))

subscriptions : Model.Model -> Sub Msg
subscriptions model =
  let
    handler =
      case model of
        Model.PreGame preGameState -> preGame preGameState
        Model.InGame _ -> (\msg -> Err (Msg.ServerProtocolError "we don't handle InGame messages yet"))
  in
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue (Json.Decode.map handler fromJS) value of
      Ok msg -> msg
      Err error -> Err (Msg.DriverProtocolError (Json.Decode.errorToString error))
