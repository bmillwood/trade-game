{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Void
import System.Environment (getArgs)

import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as WaiStatic
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.WebSockets as WS

import Game
import Protocol

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
  state@ServerState{ broadcast } <- newServerState
  _ <- forkIO . forever $ do
    msg <- readChan broadcast
    print ("broadcast", msg)
  Warp.runEnv 45286 (waiApp state staticPath)

waiApp :: ServerState -> FilePath -> Wai.Application
waiApp state staticPath =
  WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp state) (staticApp staticPath)

staticApp :: FilePath -> Wai.Application
staticApp staticPath = WaiStatic.staticApp (WaiStatic.defaultFileServerSettings staticPath)

wsApp :: ServerState -> WS.ServerApp
wsApp state = handleConnection state <=< WS.acceptRequest

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
  print "handleConnection"
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{ loginRequestName, kind } ->
      WS.withPingThread conn 30 (print ("ping", loginRequestName)) $ do
        clientBroadcast <- dupChan broadcast
        loggedIn state{ broadcast = clientBroadcast } conn loginRequestName kind
    Just other -> putStrLn ("unexpected message: " ++ show other)

sendView :: WS.Connection -> String -> GameState -> IO ()
sendView conn username game =
  sendToClient conn (UpdatePlayers (viewForPlayer game username))

sendBroadcast :: ServerState -> InternalBroadcast -> IO ()
sendBroadcast ServerState{ broadcast } msg = do
  writeChan broadcast msg

loggedIn :: ServerState -> WS.Connection -> String -> ClientKind -> IO ()
loggedIn state@ServerState{ gameStore } conn username kind = do
  print ("logged in", username, kind)
  case kind of
    Spectator -> do
      (readDoesNotReturn, neitherDoesWrite) <- concurrently
        (spectatorRead conn username)
        (writeThread state conn username)
        `onException` do
          print ("disconnected", username)
      () <- absurd readDoesNotReturn
      absurd neitherDoesWrite
    Player -> do
      modifyMVar_ gameStore $ \game -> do
        let newGame = addPlayer username game
        sendBroadcast state (Refresh newGame)
        return newGame
      (readDoesNotReturn, neitherDoesWrite) <- concurrently
        (playerRead state conn username)
        (writeThread state conn username)
        `onException` do
          modifyMVar_ gameStore $ \game -> do
            print ("disconnected", username)
            let newGame = removePlayer username game
            sendBroadcast state (Refresh newGame)
            return newGame
      () <- absurd readDoesNotReturn
      absurd neitherDoesWrite

spectatorRead :: WS.Connection -> String -> IO Void
spectatorRead conn username = forever $ do
  msg <- readFromClient conn
  print ("unexpected message from spectator", username, msg)

playerRead :: ServerState -> WS.Connection -> String -> IO Void
playerRead state@ServerState{ gameStore } conn username = forever $ do
  msg <- readFromClient conn
  case msg of
    Nothing -> return ()
    Just LoginRequest{} -> print ("unexpected second login", username, msg)
    Just (MadeChoices choices) -> do
      print ("choices", username)
      game <- fmap (setChoices username choices) (takeMVar gameStore)
      case executeTurnIfReady game of
        Nothing -> do
          putStrLn "not all ready"
          sendBroadcast state (Ready username)
          putMVar gameStore game
        Just newGame -> do
          putStrLn "executing turn"
          sendBroadcast state (Refresh newGame)
          putMVar gameStore newGame

writeThread :: ServerState -> WS.Connection -> String -> IO Void
writeThread ServerState{ gameStore, broadcast } conn username = do
  withMVar gameStore (sendView conn username)
  forever $ do
    msg <- readChan broadcast
    case msg of
      Ready playerName -> do
        print ("send ready", playerName, "to", username)
        sendToClient conn PlayerReady{ playerName, isReady = True }
      Refresh newGame -> do
        print ("refresh", username)
        sendView conn username newGame
