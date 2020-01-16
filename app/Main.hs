module Main where

import Program
import Type
import Connect
import Control.Concurrent
import Env
import System.Environment
import Init.Type
import Control.SSH.Tunnel (SshTunnel, closeSshTunnel)
import Database.MySQL.Simple (Connection, close)




verifyConnection :: Either InitError () -> InitState -> IO (Either String (SshTunnel, Connection))
verifyConnection result state = do
  case result of
    Left e -> return $ Left (show e)
    Right () -> case state of
      InitState Nothing Nothing _ -> return $  Left ("Connections lost")
      InitState (Just tunnelID) Nothing _ -> do
        closeSshTunnel tunnelID
        return $ Left ("MySQL Connections lost")
      InitState Nothing (Just sqlConnect) _ -> do
        close sqlConnect
        return $ Left ("SshTunnel Connections lost")
      InitState (Just tunnelID) (Just sqlConnect) _  -> return $ Right (tunnelID, sqlConnect)

main :: IO ()
main = do
  tid <- myThreadId
  args <- getArgs
  -- load environment variables
  case args of
    [] -> useEnv Nothing
    (s:_) -> useEnv $ Just s
  -- Init: create connection
  (connectResult, state) <- runInit tid connectAll
  connection <- verifyConnection connectResult state
  case connection of
    Left reason -> do
      putStrLn reason
    Right (tunnelID, sqlConnect) -> program (ProgramEnv tunnelID sqlConnect)
