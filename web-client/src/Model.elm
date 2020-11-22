module Model exposing (..)

import Dict exposing (Dict)

import ByDir exposing (ByDir)
import ByResource exposing (ByResource)
import Resource exposing (Resource)
import Trade exposing (Order)

type alias ResourceInfo qty = { held : qty, increment : qty, upgradeIn : qty }

type alias PlayerInfo =
  { username : String
  , ready : Bool
  , resources : ByResource (ResourceInfo Float)
  , trade : ByDir (Maybe (Order Float Float))
  }

type alias Choices =
  { action : Maybe Resource
  , tradeMined : ByDir (Order String String)
  }

newChoices : Choices
newChoices =
  { action = Nothing
  , tradeMined = ByDir.both { price = "", size = "" }
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
  | Failed

type alias PreGameState =
  { loginState : LoginState
  , loginForm : LoginForm
  }

type State
  = PreGame PreGameState
  | InGame Game

type alias Model =
  { error : Maybe String
  , state : State
  }
