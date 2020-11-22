port module Ports exposing (..)

import Dict
import Json.Decode
import Json.Encode
import Maybe

import ByDir exposing (ByDir)
import ByResource exposing (ByResource)
import Model
import Msg exposing (Msg)
import Resource exposing (Resource)
import Trade exposing (Order)

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
    Resource.Smelted -> Json.Encode.string "Smelted"

encodeByResource : (a -> Json.Encode.Value) -> ByResource a -> Json.Encode.Value
encodeByResource encode { mined, smelted } =
  Json.Encode.object
    [ ( "mined", encode mined )
    , ( "smelted", encode smelted )
    ]

encodeByDir : (a -> Json.Encode.Value) -> ByDir a -> Json.Encode.Value
encodeByDir encode { buy, sell } =
  Json.Encode.object
    [ ( "buy", encode buy )
    , ( "sell", encode sell )
    ]

encodeOrder : (px -> Json.Encode.Value) -> (qty -> Json.Encode.Value) -> Order px qty -> Json.Encode.Value
encodeOrder encodePx encodeQty { price, size } =
  Json.Encode.object
    [ ( "orderPrice", encodePx price )
    , ( "orderSize", encodeQty size )
    ]

sendChoices : Model.Choices -> Cmd msg
sendChoices { action, tradeMined } =
  Json.Encode.object
    [ ( "takeAction", encodeNullable encodeResource action )
    , ( "setTradeMined"
      , encodeByDir (encodeOrder Json.Encode.string Json.Encode.string) tradeMined
      )
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
    (Json.Decode.field "smelted" decoder)

byDir : Json.Decode.Decoder a -> Json.Decode.Decoder (ByDir a)
byDir decoder =
  Json.Decode.map2
    ByDir
    (Json.Decode.field "buy" decoder)
    (Json.Decode.field "sell" decoder)

rational : Json.Decode.Decoder Float
rational =
  Json.Decode.map2
    (\num den -> num / den)
    (Json.Decode.field "numerator" Json.Decode.float)
    (Json.Decode.field "denominator" Json.Decode.float)

resourceInfo : Json.Decode.Decoder (Model.ResourceInfo Float)
resourceInfo =
  Json.Decode.map3
    Model.ResourceInfo
    (Json.Decode.field "held" rational)
    (Json.Decode.field "increment" rational)
    (Json.Decode.field "upgradeIn" rational)

decodeOrder : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder (Order a b)
decodeOrder decodePrice decodeSize =
  Json.Decode.map2
    Order
    (Json.Decode.field "orderPrice" decodePrice)
    (Json.Decode.field "orderSize" decodeSize)

playerInfo : Json.Decode.Decoder Model.PlayerInfo
playerInfo =
  Json.Decode.map4
    Model.PlayerInfo
    (Json.Decode.field "username" Json.Decode.string)
    (Json.Decode.field "ready" Json.Decode.bool)
    (Json.Decode.field "resources" (byResource resourceInfo))
    (Json.Decode.field "trade" (byDir (Json.Decode.nullable (decodeOrder rational rational))))

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
