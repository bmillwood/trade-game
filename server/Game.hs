{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Game
  ( GameState(..)
  , initGameState
  , addPlayer
  , removePlayer
  , executeTurnIfReady
  , Choices(..)
  , setChoices
  , PlayerView(..)
  , viewForPlayer
  ) where

import qualified Data.Map as Map
import GHC.Generics

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Network.WebSockets as WS

data Resource
  = Mined
  | Crafted
  deriving (Generic, Show)

instance Aeson.FromJSON Resource

data ByResource a =
  ByResource
    { mined :: a
    , crafted :: a
    } deriving (Functor, Generic, Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (ByResource a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (ByResource a)

bothResources :: a -> ByResource a
bothResources x = ByResource { mined = x, crafted = x }

data ResourceInfo a =
  ResourceInfo
    { held :: a
    , increment :: a
    , upgradeIn :: a
    } deriving (Generic, Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (ResourceInfo a)

upgradeInFor :: (Num n) => n -> n
upgradeInFor n = 5 * n * n

produceResource :: (Num n, Ord n) => n -> ResourceInfo n -> ResourceInfo n
produceResource amount ResourceInfo{ held, increment, upgradeIn }
  | amount >= upgradeIn =
    produceResource
      (amount - upgradeIn)
      ResourceInfo{ held = held + upgradeIn, increment = increment + 1, upgradeIn = upgradeInFor (increment + 1) }
  | otherwise =
      ResourceInfo{ held = held + amount, increment, upgradeIn = upgradeIn - amount }

produce :: (Num n, Ord n) => Resource -> ByResource (ResourceInfo n) -> ByResource (ResourceInfo n)
produce Mined resources@ByResource{ mined = mined@ResourceInfo{ increment } } =
  resources{ mined = produceResource increment mined }
produce Crafted resources@ByResource{ mined, crafted } =
  case ( mined, crafted ) of
    ( ResourceInfo{ held = heldMined }, ResourceInfo{ increment } )
      -> ByResource { mined = mined{ held = heldMined - amount }, crafted = produceResource amount crafted }
      where
        amount = min heldMined increment

data TradeParam a =
  TradeParam
    { giveMax :: a
    , getForEachGive :: a
    } deriving (Generic, Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (TradeParam a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (TradeParam a)

data Choices =
  Choices
    { takeAction :: Maybe Resource
    , setTrade :: ByResource (TradeParam String)
    } deriving (Generic, Show)

instance Aeson.FromJSON Choices

data PlayerInfo =
  PlayerInfo
    { username :: String
    , ready :: Bool
    , resources :: ByResource (ResourceInfo Float)
    , trade :: ByResource (Maybe (TradeParam Float))
    } deriving (Generic, Show)

instance Aeson.ToJSON PlayerInfo

newtype GameState
  = GameState (Map.Map String (Maybe Choices, PlayerInfo))
  deriving (Show)

initGameState :: GameState
initGameState = GameState Map.empty

newPlayer :: String -> PlayerInfo
newPlayer username =
  PlayerInfo
    { username
    , ready = False
    , resources = bothResources ResourceInfo{ held = 0, increment = 1, upgradeIn = upgradeInFor 1 }
    , trade = bothResources Nothing
    }

addPlayer :: String -> GameState -> GameState
addPlayer username (GameState players) =
  GameState (Map.insert username (Nothing, newPlayer username) players)

removePlayer :: String -> GameState -> GameState
removePlayer username (GameState players) =
  GameState (Map.delete username players)

applyTradeInfo :: ByResource (TradeParam String) -> PlayerInfo -> PlayerInfo
applyTradeInfo _ player = player

doTrades :: Map.Map String PlayerInfo -> Map.Map String PlayerInfo
doTrades players = players

executeTurnIfReady :: GameState -> Maybe GameState
executeTurnIfReady unchanged@(GameState players) =
  fmap (GameState . fmap setUnready . doTrades) (traverse applyChoices players)
  where
    setUnready player = (Nothing, player{ ready = False })
    applyChoices (Nothing, player) = Nothing
    applyChoices (Just Choices{ takeAction, setTrade }, player) =
      Just (applyTradeInfo setTrade . playerProduce takeAction $ player)
    playerProduce Nothing player = player
    playerProduce (Just resource) player@PlayerInfo{ resources } = player{ resources = produce resource resources }

setChoices :: String -> Choices -> GameState -> GameState
setChoices usernameToSet choices (GameState players) =
  GameState (Map.adjust updatePlayer usernameToSet players)
  where
    updatePlayer (_, info) = (Just choices, info{ ready = True })

data PlayerView =
  PlayerView
    { me :: PlayerInfo
    , others :: [PlayerInfo]
    } deriving (Generic, Show)

instance Aeson.ToJSON PlayerView

viewForPlayer :: GameState -> String -> Maybe PlayerView
viewForPlayer (GameState players) username =
  case Map.updateLookupWithKey (\_ _ -> Nothing) username players of
    (Nothing, _) -> Nothing
    (Just (_, me), others) -> Just PlayerView{ me, others = map snd (Map.elems others) }
