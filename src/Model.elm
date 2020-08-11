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

type alias LoginForm =
  { username : String }

type alias Game =
  { me : PlayerInfo
  , others : List PlayerInfo
  }

type Model
  = PreGame LoginForm
  | InGame Game
