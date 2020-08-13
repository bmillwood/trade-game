module Model exposing (..)

import ByResource exposing (ByResource)
import Dict exposing (Dict)
import Resource exposing (Resource)

type alias ResourceInfo qty = { held : qty, increment : qty, upgradeIn : qty }

type alias TradeParam qty = { giveMax : qty, getForEachGive : qty }

type alias TradeMatrix qty = ByResource (Maybe (TradeParam qty))

type alias PlayerInfo =
  { username : String
  , ready : Bool
  , resources : ByResource (ResourceInfo Float)
  , trade : TradeMatrix Float
  }

type alias Choices =
  { action : Maybe Resource
  , trade : ByResource (TradeParam String)
  }

type alias Players =
  { me : PlayerInfo
  , others : List PlayerInfo
  }

type alias Game =
  { choices : Choices
  , players : Players
  }

type alias LoginForm =
  { endpoint : String
  , username : String
  }

type LoginState
  = NotSubmitted
  | Waiting
  | Failed String

type alias PreGameState =
  { loginState : LoginState
  , loginForm : LoginForm
  }

type Model
  = PreGame PreGameState
  | InGame Game
