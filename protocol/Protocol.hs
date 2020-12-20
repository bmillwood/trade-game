{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Protocol where

import Control.Lens
import qualified Data.Aeson as Aeson
import GHC.Generics

data Dir
  = Buy
  | Sell
  deriving (Generic, Show)

data ByDir a =
  ByDir
    { buy :: a
    , sell :: a
    } deriving (Functor, Generic, Show)

byDir :: (Functor f) => Dir -> (a -> f a) -> ByDir a -> f (ByDir a)
byDir Buy  afa by = fmap (\a -> by{ buy  = a }) (afa (buy  by))
byDir Sell afa by = fmap (\a -> by{ sell = a }) (afa (sell by))

createByDir :: (Dir -> a) -> ByDir a
createByDir f = ByDir { buy = f Buy, sell = f Sell }

instance Applicative ByDir where
  pure = createByDir . const
  byf <*> byx = createByDir $ \dir -> (byf ^. byDir dir) (byx ^. byDir dir)

instance (Aeson.ToJSON a) => Aeson.ToJSON (ByDir a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (ByDir a)

type Price = Rational
type Qty = Rational

data Order px qty =
  Order
    { orderPrice :: px
    , orderSize :: qty
    } deriving (Generic, Show)

instance (Aeson.ToJSON a, Aeson.ToJSON b) => Aeson.ToJSON (Order a b)
instance (Aeson.FromJSON a, Aeson.FromJSON b) => Aeson.FromJSON (Order a b)

data Resource
  = Mined
  | Smelted
  deriving (Generic, Show)

instance Aeson.FromJSON Resource
instance Aeson.ToJSON Resource

data ByResource a =
  ByResource
    { mined :: a
    , smelted :: a
    } deriving (Functor, Generic, Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (ByResource a)
instance (Aeson.FromJSON a) => Aeson.FromJSON (ByResource a)

createByResource :: (Resource -> a) -> ByResource a
createByResource f = ByResource { mined = f Mined, smelted = f Smelted }

byResource :: (Functor f) => Resource -> (a -> f a) -> ByResource a -> f (ByResource a)
byResource Mined   afa by = fmap (\a -> by{ mined   = a }) (afa (mined   by))
byResource Smelted afa by = fmap (\a -> by{ smelted = a }) (afa (smelted by))

instance Applicative ByResource where
  pure x = ByResource{ mined = x, smelted = x }
  byf <*> byx = createByResource $ \r -> (byf ^. byResource r) (byx ^. byResource r)

data ResourceInfo a =
  ResourceInfo
    { held :: a
    , increment :: a
    , upgradeIn :: a
    } deriving (Generic, Show)

instance (Aeson.FromJSON a) => Aeson.FromJSON (ResourceInfo a)
instance (Aeson.ToJSON a) => Aeson.ToJSON (ResourceInfo a)

data Choices =
  Choices
    { takeAction :: Maybe Resource
    , setTradeMined :: ByDir (Order String String)
    } deriving (Generic, Show)

instance Aeson.FromJSON Choices
instance Aeson.ToJSON Choices

data PlayerInfo =
  PlayerInfo
    { username :: String
    , ready :: Bool
    , resources :: ByResource (ResourceInfo Rational)
    , trade :: ByDir (Maybe (Order Price Qty))
    } deriving (Generic, Show)

instance Aeson.FromJSON PlayerInfo
instance Aeson.ToJSON PlayerInfo

data PlayerView =
  PlayerView
    { me :: PlayerInfo
    , others :: [PlayerInfo]
    } deriving (Generic, Show)

instance Aeson.FromJSON PlayerView
instance Aeson.ToJSON PlayerView

data FromClient
  = LoginRequest { loginRequestName :: String }
  | MadeChoices Choices
  deriving (Generic, Show)

instance Aeson.FromJSON FromClient
instance Aeson.ToJSON FromClient

data ToClient
  = UpdatePlayers PlayerView
  | PlayerReady { playerName :: String, isReady :: Bool }
  deriving (Generic, Show)

instance Aeson.FromJSON ToClient
instance Aeson.ToJSON ToClient
