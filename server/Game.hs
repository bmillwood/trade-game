{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Maybe (listToMaybe)
import qualified Data.Map as Map
import GHC.Generics
import Numeric (readFloat)

import qualified Data.Aeson as Aeson

import Trade

data Resource
  = Mined
  | Smelted
  deriving (Generic, Show)

instance Aeson.FromJSON Resource

data ByResource a =
  ByResource
    { mined :: a
    , smelted :: a
    } deriving (Functor, Generic, Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (ByResource a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (ByResource a)

bothResources :: a -> ByResource a
bothResources x = ByResource { mined = x, smelted = x }

byResource :: (Functor f) => Resource -> (a -> f a) -> ByResource a -> f (ByResource a)
byResource Mined   afa by = fmap (\a -> by{ mined   = a }) (afa (mined   by))
byResource Smelted afa by = fmap (\a -> by{ smelted = a }) (afa (smelted by))

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
produce Smelted ByResource{ mined, smelted } =
  case ( mined, smelted ) of
    ( ResourceInfo{ held = heldMined }, ResourceInfo{ increment } )
      -> ByResource { mined = mined{ held = heldMined - amount }, smelted = produceResource amount smelted }
      where
        amount = min heldMined increment

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
    , resources :: ByResource (ResourceInfo Rational)
    , trade :: ByResource (Maybe (TradeParam Rational))
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
applyTradeInfo tradeInfo player = player{ trade = fmap parseTradeParam tradeInfo }
  where
    parseTradeParam TradeParam{ giveMax, getForEachGive } =
      liftA2 TradeParam (readPositive giveMax) (readPositive getForEachGive)
    readPositive str = do
      result <- fmap fst . listToMaybe $ readFloat str
      guard (result > 0)
      return result

applyAuctionResult :: AuctionResult String -> Map.Map String PlayerInfo -> Map.Map String PlayerInfo
applyAuctionResult AuctionResult{ price, leftsTraded, rightsTraded } players =
  foldr (applyTrades Mined) (foldr (applyTrades Smelted) players rightsTraded) leftsTraded
  where
    other Mined = Smelted
    other Smelted = Mined
    applyTrades give (who, qty) =
      Map.adjust
        (applyTrade give TradeParam{ giveMax = qty, getForEachGive = prices ^. byResource give })
        who
    applyTrade give TradeParam{ giveMax, getForEachGive } info@PlayerInfo{ resources } =
      info{
        resources =
          resources
          & byResource give %~ addResource (negate giveMax)
          & byResource (other give) %~ addResource (getForEachGive * giveMax)
        }
    addResource qty resource@ResourceInfo{ held } = resource{ held = held + qty }
    prices =
      case price of
        Left px -> ByResource{ mined = px, smelted = recip px }
        Right px -> ByResource{ mined = recip px, smelted = px }

doTrades :: Map.Map String PlayerInfo -> Map.Map String PlayerInfo
doTrades players =
  maybe id applyAuctionResult
    (runAuction (paramsByResource Mined) (paramsByResource Smelted))
    players
  where
    paramsByResource resource =
      Map.mapMaybe (\PlayerInfo{ trade } -> trade ^. byResource resource) players
      & Map.toList

executeTurnIfReady :: GameState -> Maybe GameState
executeTurnIfReady (GameState players) =
  fmap (GameState . fmap setUnready . doTrades) (traverse applyChoices players)
  where
    setUnready player = (Nothing, player{ ready = False })
    applyChoices (Nothing, _) = Nothing
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
