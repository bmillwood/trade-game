module Model exposing (..)

import Dict exposing (Dict)
import Resource exposing (Resource)

type alias ByResource a = { mined : a, crafted : a }

type alias ResourceInfo qty = { held : qty, increment : qty, upgradeIn : qty }

type alias TradeParam qty = { giveMax : qty, getForEachGive : qty }

type alias TradeMatrix qty = ByResource (Maybe (TradeParam qty))

type alias PlayerInfo qty =
  { username : String
  , ready : Bool
  , resources : ByResource (ResourceInfo qty)
  , trade : TradeMatrix qty
  }

type alias Model =
  { me : PlayerInfo Float
  , others : List (PlayerInfo Float)
  }
