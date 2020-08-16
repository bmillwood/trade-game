{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Void
import GHC.Generics
import System.Environment (getArgs)
import System.IO.Error

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import Game

data InternalBroadcast
  = Ready String
  | Refresh GameState
  deriving (Show)

data ServerState
  = ServerState{ gameStore :: MVar GameState, broadcast :: Chan InternalBroadcast }

newServerState :: IO ServerState
newServerState = ServerState <$> newMVar initGameState <*> newChan

main :: IO ()
main = do
  [staticPath] <- getArgs
  state <- newServerState
  Warp.runEnv 45286 (waiApp state staticPath)

waiApp :: ServerState -> FilePath -> Wai.Application
waiApp state staticPath =
  WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp state) (staticApp staticPath)

staticApp :: FilePath -> Wai.Application
staticApp staticPath = WaiStatic.staticApp (WaiStatic.defaultFileServerSettings staticPath)

wsApp :: ServerState -> WS.ServerApp
wsApp state = handleConnection state <=< WS.acceptRequest

data FromClient
  = LoginRequest { loginRequestName :: String }
  | MadeChoices Choices
  deriving (Generic, Show)

instance Aeson.FromJSON FromClient

data ToClient
  = UpdatePlayers PlayerView
  | PlayerReady { playerName :: String, isReady :: Bool }
  deriving (Generic, Show)

instance Aeson.ToJSON ToClient

sendToClient :: WS.Connection -> ToClient -> IO ()
sendToClient conn msg = do
  WS.sendTextData conn (Aeson.encode msg)

readFromClient :: WS.Connection -> IO (Maybe FromClient)
readFromClient conn = do
  json <- WS.receiveData conn
  case Aeson.decode json of
    Nothing -> do
      putStrLn ("couldn't decode: " ++ show json)
      return Nothing
    Just msg -> return (Just msg)

handleConnection :: ServerState -> WS.Connection -> IO ()
handleConnection state@ServerState{ broadcast } conn = do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{ loginRequestName } -> do
      broadcast <- dupChan broadcast
      loggedIn state conn loginRequestName
    Just other -> putStrLn ("unexpected message: " ++ show other)

sendView :: WS.Connection -> String -> GameState -> IO ()
sendView conn username game =
  case viewForPlayer game username of
    Nothing -> return ()
    Just view -> sendToClient conn (UpdatePlayers view)

loggedIn :: ServerState -> WS.Connection -> String -> IO ()
loggedIn state@ServerState{ gameStore, broadcast } conn username = do
  modifyMVar_ gameStore $ \game -> do
    let newGame = addPlayer username game
    writeChan broadcast (Refresh newGame)
    return newGame
  (readDoesNotReturn, neitherDoesWrite) <- concurrently
    (readThread state conn username)
    (writeThread state conn username)
    `onException` do
    modifyMVar_ gameStore $ \game -> do
      let newGame = removePlayer username game
      writeChan broadcast (Refresh newGame)
      return newGame
  absurd readDoesNotReturn
  absurd neitherDoesWrite

readThread :: ServerState -> WS.Connection -> String -> IO Void
readThread ServerState{ gameStore, broadcast } conn username = forever $ do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{} -> putStrLn ("unexpected second login: " ++ show msg)
    Just (MadeChoices choices) -> do
      print (username, choices)
      game <- fmap (setChoices username choices) (takeMVar gameStore)
      case executeTurnIfReady game of
        Nothing -> do
          putStrLn "not all ready"
          writeChan broadcast (Ready username)
          putMVar gameStore game
        Just newGame -> do
          putStrLn "executing turn"
          writeChan broadcast (Refresh newGame)
          putMVar gameStore newGame

writeThread :: ServerState -> WS.Connection -> String -> IO Void
writeThread ServerState{ gameStore, broadcast } conn username = do
  withMVar gameStore (sendView conn username)
  forever $ do
    msg <- readChan broadcast
    print ("broadcast", username, msg)
    case msg of
      Ready playerName ->
        sendToClient conn PlayerReady{ playerName, isReady = True }
      Refresh newGame ->
        sendView conn username newGame
