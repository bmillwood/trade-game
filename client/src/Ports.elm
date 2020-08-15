port module Ports exposing (..)

import Dict
import Json.Decode
import Json.Encode
import Maybe

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

contentsWithTag : String -> Json.Encode.Value -> Json.Encode.Value
contentsWithTag tag contents =
  Json.Encode.object
    [ ( "tag", Json.Encode.string tag )
    , ( "contents", contents )
    ]

login : { username : String } -> Cmd msg
login { username } =
  Json.Encode.object
    [ ( "tag", Json.Encode.string "LoginRequest" )
    , ( "loginRequestName", Json.Encode.string username )
    ]
  |> send

encodeNullable : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeNullable encode m = Maybe.withDefault Json.Encode.null (Maybe.map encode m)

encodeResource : Resource -> Json.Encode.Value
encodeResource resource =
  case resource of
    Resource.Mined -> Json.Encode.string "Mined"
    Resource.Crafted -> Json.Encode.string "Crafted"

encodeByResource : (a -> Json.Encode.Value) -> ByResource a -> Json.Encode.Value
encodeByResource encode { mined, crafted } =
  Json.Encode.object
    [ ( "mined", encode mined )
    , ( "crafted", encode crafted )
    ]

encodeTradeParam : (qty -> Json.Encode.Value) -> Model.TradeParam qty -> Json.Encode.Value
encodeTradeParam encode { giveMax, getForEachGive } =
  Json.Encode.object
    [ ( "giveMax", encode giveMax )
    , ( "getForEachGive", encode getForEachGive )
    ]

sendChoices : Model.Choices -> Cmd msg
sendChoices { action, trade } =
  Json.Encode.object
    [ ( "takeAction", encodeNullable encodeResource action )
    , ( "setTrade", encodeByResource (encodeTradeParam Json.Encode.string) trade )
    ]
  |> contentsWithTag "MadeChoices"
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
  | WithFieldsInline (Json.Decode.Decoder a)
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
         Just (WithFieldsInline decoder) -> decoder
         Just (WithContents decoder) -> Json.Decode.field "contents" decoder)

type ServerMsg
  = UpdatePlayers Model.Players
  | PlayerReady String Bool

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
    ByResource
    (Json.Decode.field "mined" decoder)
    (Json.Decode.field "crafted" decoder)

tradeParam : Json.Decode.Decoder a -> Json.Decode.Decoder (Model.TradeParam a)
tradeParam decoder =
  Json.Decode.map2
    Model.TradeParam
    (Json.Decode.field "giveMax" decoder)
    (Json.Decode.field "getForEachGive" decoder)

resourceInfo : Json.Decode.Decoder (Model.ResourceInfo Float)
resourceInfo =
  Json.Decode.map3
    Model.ResourceInfo
    (Json.Decode.field "held" Json.Decode.float)
    (Json.Decode.field "increment" Json.Decode.float)
    (Json.Decode.field "upgradeIn" Json.Decode.float)

playerInfo : Json.Decode.Decoder Model.PlayerInfo
playerInfo =
  Json.Decode.map4
    Model.PlayerInfo
    (Json.Decode.field "username" Json.Decode.string)
    (Json.Decode.field "ready" Json.Decode.bool)
    (Json.Decode.field "resources" (byResource resourceInfo))
    (Json.Decode.field "trade" (byResource (Json.Decode.nullable (tradeParam Json.Decode.float))))

players : Json.Decode.Decoder Model.Players
players =
  Json.Decode.map2
    Model.Players
    (Json.Decode.field "me" playerInfo)
    (Json.Decode.field "others" (Json.Decode.list playerInfo))

serverMsg : Json.Decode.Decoder ServerMsg
serverMsg =
  let
    playerReady =
      Json.Decode.map2
        PlayerReady
        (Json.Decode.field "playerName" Json.Decode.string)
        (Json.Decode.field "isReady" Json.Decode.bool)
  in
  variant
    { name = "serverMsg" }
    [ ( "UpdatePlayers", WithContents (Json.Decode.map UpdatePlayers players) )
    , ( "PlayerReady", WithFieldsInline playerReady )
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
    FromServer (UpdatePlayers newPlayers) -> Ok (Msg.PreGame (Msg.Accepted newPlayers))
    FromServer (PlayerReady playerName isReady) -> Err (Msg.ServerProtocolError "PlayerReady before we entered a game")

inGame : Model.Game -> FromJS -> Msg
inGame model msg =
  case msg of
    ServerStatus Connected -> Err (Msg.DriverProtocolError "Received Connected but we are already connected?")
    ServerStatus Disconnected -> Err Msg.ServerDisconnected
    FromServer (UpdatePlayers newPlayers) -> Ok (Msg.InGame (Msg.ServerUpdate newPlayers))
    FromServer (PlayerReady playerName isReady) -> Ok (Msg.InGame (Msg.SetOtherReady playerName isReady))

subscriptions : Model.Model -> Sub Msg
subscriptions model =
  let
    handler =
      case model.state of
        Model.PreGame preGameState -> preGame preGameState
        Model.InGame gameState -> inGame gameState
  in
  receiveFromJS <| \value ->
    case Json.Decode.decodeValue (Json.Decode.map handler fromJS) value of
      Ok msg -> msg
      Err error -> Err (Msg.DriverProtocolError (Json.Decode.errorToString error))
