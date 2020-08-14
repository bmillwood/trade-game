module LocationParser exposing (..)

import Dict
import Json.Decode
import Result

import Url
import Url.Parser
import Url.Parser.Query

type alias QueryParams = { username : Maybe String, autoLogin : Maybe Bool }

queryParser =
  Url.Parser.Query.map2
    QueryParams
    (Url.Parser.Query.string "username")
    (Url.Parser.Query.enum "autoLogin" (Dict.fromList [ ("true", True), ("false", False) ]))

defaults = { error = Nothing, endpoint = "ws://localhost:45286", username = "", autoLogin = False }

parseLocationProperly : Json.Decode.Value -> { error : Maybe String, endpoint : String, username : String, autoLogin : Bool }
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
    Ok { username, autoLogin } ->
      { defaults
      | username = Maybe.withDefault defaults.username username
      , autoLogin = Maybe.withDefault defaults.autoLogin autoLogin
      }

parseLocationWorkaround : Json.Decode.Value -> { error : Maybe String, endpoint : String, username : String, autoLogin : Bool }
parseLocationWorkaround flags =
  let
    parsedUrl =
      Json.Decode.decodeValue (Json.Decode.at ["location", "search"] Json.Decode.string) flags
      |> Result.mapError Json.Decode.errorToString
  in
  case parsedUrl of
    Err msg ->
      { defaults | error = Just msg }
    Ok search ->
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
      }

parseLocation = parseLocationWorkaround
