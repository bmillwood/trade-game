{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Game where

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
    } deriving (Generic, Show)

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

data PlayerView =
  PlayerView
    { me :: PlayerInfo
    , others :: [PlayerInfo]
    } deriving (Generic, Show)

instance Aeson.ToJSON PlayerView

newPlayer :: String -> PlayerInfo
newPlayer username =
  PlayerInfo
    { username
    , ready = False
    , resources = bothResources ResourceInfo{ held = 0, increment = 1, upgradeIn = 1 }
    , trade = bothResources Nothing
    }
