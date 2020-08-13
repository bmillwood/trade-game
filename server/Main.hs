{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Monad
import GHC.Generics

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS

main :: IO ()
main = WS.runServer "localhost" 45286 app

app :: WS.ServerApp
app = handleConnection <=< WS.acceptRequest

data LoginRequest = LoginRequest { username :: String } deriving (Generic, Show)

instance Aeson.FromJSON LoginRequest where

handleConnection :: WS.Connection -> IO ()
handleConnection conn = do
  login <- WS.receiveData conn
  case Aeson.decode login of
    Nothing -> putStrLn "couldn't decode"
    Just LoginRequest{ username } -> do
      WS.sendTextData conn (Aeson.encode ())
      putStrLn ("username: " ++ username)
