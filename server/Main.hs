{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import GHC.Generics
import System.IO.Error

import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Network.WebSockets as WS

import Game

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

data InternalBroadcast
  = Ready String
  | Refresh GameState
  deriving (Show)

main :: IO ()
main = do
  state <- newMVar initGameState
  broadcast <- newChan
  retryResourceBusy 60 (WS.runServer "localhost" 45286 (app state broadcast))

app :: MVar GameState -> Chan InternalBroadcast -> WS.ServerApp
app state broadcast = handleConnection state broadcast <=< WS.acceptRequest

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

handleConnection :: MVar GameState -> Chan InternalBroadcast -> WS.Connection -> IO ()
handleConnection state broadcast conn = do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{ loginRequestName } -> do
      broadcast <- dupChan broadcast
      loggedIn state broadcast conn loginRequestName
    Just other -> putStrLn ("unexpected message: " ++ show other)

sendView :: WS.Connection -> String -> GameState -> IO ()
sendView conn username game =
  case viewForPlayer game username of
    Nothing -> return ()
    Just view -> sendToClient conn (UpdatePlayers view)

loggedIn :: MVar GameState -> Chan InternalBroadcast -> WS.Connection -> String -> IO ()
loggedIn state broadcast conn username = do
  modifyMVar_ state $ \game -> do
    let newGame = addPlayer username game
    writeChan broadcast (Refresh newGame)
    return newGame
  _ <- forkIO $ readThread state broadcast conn username
  writeThread state broadcast conn username

readThread :: MVar GameState -> Chan InternalBroadcast -> WS.Connection -> String -> IO ()
readThread state broadcast conn username = forever $ do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{} -> putStrLn ("unexpected second login: " ++ show msg)
    Just (MadeChoices choices) -> do
      print (username, choices)
      game <- fmap (setChoices username choices) (takeMVar state)
      case executeTurnIfReady game of
        Nothing -> do
          putStrLn "not all ready"
          writeChan broadcast (Ready username)
          putMVar state game
        Just newGame -> do
          putStrLn "executing turn"
          writeChan broadcast (Refresh newGame)
          putMVar state newGame

writeThread :: MVar GameState -> Chan InternalBroadcast -> WS.Connection -> String -> IO ()
writeThread state broadcast conn username = do
  withMVar state (sendView conn username)
  forever $ do
    msg <- readChan broadcast
    print ("broadcast", username, msg)
    case msg of
      Ready playerName ->
        sendToClient conn PlayerReady{ playerName, isReady = True }
      Refresh newGame ->
        sendView conn username newGame
