{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import GHC.Generics
import System.IO.Error

import qualified Data.Aeson as Aeson
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
      threadDelay 1000000
      putStrLn "finished waiting"
