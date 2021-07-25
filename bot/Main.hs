{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Client as WSC

import Protocol

main :: IO ()
main =
  WSC.runClient
    "127.0.0.1"
    45286
    ""
    clientApp

sendToServer :: WS.Connection -> FromClient -> IO ()
sendToServer conn msg =
  WS.sendTextData conn (Aeson.encode msg)

readFromServer :: WS.Connection -> IO (Maybe ToClient)
readFromServer conn = do
  json <- WS.receiveData conn
  case Aeson.decode json of
    Nothing -> do
      putStrLn ("couldn't decode: " ++ show json)
      return Nothing
    Just msg -> return (Just msg)

clientApp :: WSC.ClientApp ()
clientApp conn = do
  putStrLn "clientApp"
  sendToServer conn LoginRequest{ loginRequestName = "minebot", kind = Player }
  alternate Mined Smelted
  where
    alternate thisTurn nextTurn = do
      sendToServer conn
        (MadeChoices Choices{ takeAction = Just thisTurn, setTradeMined = pure (Order "" "") })
      waitForNextChoices
      alternate nextTurn thisTurn
    waitForNextChoices = do
      Just msg <- readFromServer conn
      case msg of
        PlayerReady{} -> waitForNextChoices
        UpdatePlayers GameView{ me, others = _ } ->
          case me of
            Just PlayerInfo{ ready = True } -> waitForNextChoices
            Just PlayerInfo{ ready = False } -> return ()
            Nothing -> waitForNextChoices

