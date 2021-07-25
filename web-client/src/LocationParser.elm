module LocationParser exposing (..)

import Dict
import Json.Decode
import Result

import Url
import Url.Parser
import Url.Parser.Query

type alias QueryParams = { username : Maybe String, autoLogin : Maybe Bool, spectate : Maybe Bool }

queryParser =
  let
    bool name =
      Url.Parser.Query.enum name (Dict.fromList [ ("true", True), ("false", False) ])
  in
  Url.Parser.Query.map3
    QueryParams
    (Url.Parser.Query.string "username")
    (bool "autoLogin")
    (bool "spectate")

defaults =
  { error = Nothing
  , endpoint = "ws://localhost:45286"
  , username = ""
  , autoLogin = False
  , spectate = False
  }

parseLocationProperly : Json.Decode.Value -> { error : Maybe String, endpoint : String, username : String, autoLogin : Bool, spectate : Bool }
parseLocationProperly flags =
  let
    parsedUrl =
      Json.Decode.decodeValue (Json.Decode.at ["location", "href"] Json.Decode.string) flags
      |> Result.mapError Json.Decode.errorToString
      |> Result.andThen (\href ->
           Url.fromString href
           |> Result.fromMaybe ("Url parsing failed on: " ++ href))
      |> Result.andThen (\url ->
           Url.Parser.parse (Url.Parser.query queryParser) url
           |> Result.fromMaybe ("Query parser failed on url: " ++ Url.toString url))
  in
  case parsedUrl of
    Err msg ->
      { defaults | error = Just msg }
    Ok { username, autoLogin, spectate } ->
      { defaults
      | username = Maybe.withDefault defaults.username username
      , autoLogin = Maybe.withDefault defaults.autoLogin autoLogin
      , spectate = Maybe.withDefault defaults.spectate spectate
      }

parseLocationWorkaround : Json.Decode.Value -> { error : Maybe String, endpoint : String, username : String, autoLogin : Bool, spectate : Bool }
parseLocationWorkaround flags =
  let
    ofParts host pathname search =
      let
        pieces = String.split "&" (String.dropLeft 1 search)
        getValue key kv =
          if String.startsWith (key ++ "=") kv
          then Just (String.dropLeft (String.length key + 1) kv)
          else Nothing
        lookup key =
          List.filterMap (getValue key) pieces
          |> List.head
      in
      { defaults
      | username = Maybe.withDefault defaults.username (lookup "username")
      , autoLogin = List.member "autoLogin=true" pieces
      , spectate = List.member "spectate=true" pieces
      , endpoint = "ws://" ++ host ++ pathname
      }
    decoder =
      Json.Decode.field "location"
      <| Json.Decode.map3 ofParts
          (Json.Decode.field "host" Json.Decode.string)
          (Json.Decode.field "pathname" Json.Decode.string)
          (Json.Decode.field "search" Json.Decode.string)
    parseResult =
      Json.Decode.decodeValue decoder flags
      |> Result.mapError Json.Decode.errorToString
  in
  case parseResult of
    Err msg ->
      { defaults | error = Just msg }
    Ok result ->
      result

parseLocation = parseLocationWorkaround
