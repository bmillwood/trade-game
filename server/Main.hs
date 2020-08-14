{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Generics
import System.IO.Error

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Network.WebSockets as WS

retryResourceBusy :: Int -> IO () -> IO ()
retryResourceBusy retries io =
  if retries <= 0
  then io
  else do
    r <- tryJust (guard . isAlreadyInUseError) io
    case r of
      Left _ -> do
        putStrLn ("already in use error, waiting 1s and trying " ++ show retries ++ " more times")
        threadDelay 1000000
        retryResourceBusy (retries - 1) io
      Right v -> return v

main :: IO ()
main = retryResourceBusy 60 (WS.runServer "localhost" 45286 app)

app :: WS.ServerApp
app = handleConnection <=< WS.acceptRequest

data LoginRequest = LoginRequest { loginRequestName :: String } deriving (Generic, Show)

instance Aeson.FromJSON LoginRequest where
  parseJSON = Aeson.withObject "LoginRequest" $ \v ->
    LoginRequest <$> v .: "username"

data ByResource a =
  ByResource
    { mined :: a
    , crafted :: a
    } deriving (Generic, Show)

instance (Aeson.ToJSON a) => Aeson.ToJSON (ByResource a)

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

data PlayerInfo =
  PlayerInfo
    { username :: String
    , ready :: Bool
    , resources :: ByResource (ResourceInfo Float)
    , trade :: ByResource (Maybe (TradeParam Float))
    } deriving (Generic, Show)

instance Aeson.ToJSON PlayerInfo

data Players =
  Players
    { me :: PlayerInfo
    , others :: [PlayerInfo]
    } deriving (Generic, Show)

instance Aeson.ToJSON Players

data ToClient
  = UpdatePlayers Players
  | PlayerReady { playerName :: String, isReady :: Bool }
  deriving (Generic, Show)

instance Aeson.ToJSON ToClient

mockPlayers :: Players
mockPlayers = Players { me, others = [] }
  where
    me =
      PlayerInfo
        { username = "bm"
        , ready = False
        , resources = bothResources ResourceInfo{ held = 0, increment = 1, upgradeIn = 1 }
        , trade = bothResources Nothing
        }

handleConnection :: WS.Connection -> IO ()
handleConnection conn = do
  login <- WS.receiveData conn
  case Aeson.decode login of
    Nothing -> putStrLn "couldn't decode"
    Just LoginRequest{ loginRequestName } -> do
      WS.sendTextData conn (Aeson.encode (UpdatePlayers mockPlayers))
      forever (threadDelay 1000000)
