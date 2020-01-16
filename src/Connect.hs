{-# LANGUAGE OverloadedStrings #-}
module Connect where

import Database.MySQL.Simple
import Control.SSH.Tunnel
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Except
import Control.Exception (try)
import System.Posix.Signals
import Filesystem.Path.CurrentOS (decodeString)
import Network.HTTP.Client (defaultManagerSettings)
import Data.Text
import Parser

import Init.Type




tunnelConfig :: InitM SshTunnelConfig
tunnelConfig = do
  filePath <- getEnv "PEM_FILEPATH"
  localPort <- getIntEnv "LOCAL_PORT"
  sqlPort <- getIntEnv "SQL_PORT"
  sshName <- getEnv "SSH_NAME"
  sshHost <- getEnv "SSH_HOST"

  return $ SshTunnelConfig
    (pack filePath) -- Path to ssh pem file
    localPort -- Port that is used for local ssh proxy
    sqlPort -- Port that is used on remote machine for vpn manager
    (pack sshName) -- Name of SSH user for tunneling
    (pack sshHost) -- Host of SSH tunnel
    "./ssh-temp" -- Place where we can place our temp files

connectTunnel :: InitM SshTunnel
connectTunnel = do
  filePath <- decodeString <$> getEnv "SSH_TUNNELINFO"
  tunnelConfig' <- tunnelConfig
  (_, tunnel) <- liftIO $ makeSshTunnel tunnelConfig' defaultManagerSettings
  liftIO $ saveSshTunnel filePath tunnel
  modify $ \s -> s { sshTunnel = Just tunnel }
  return tunnel


sqlInfo :: InitM ConnectInfo
sqlInfo = do
  connectHost' <- getEnv "HOST"
  connectPort' <- fromIntegral <$> getIntEnv "LOCAL_PORT"
  connectUser' <- getEnv "SQL_USER"
  connectPassword' <- getEnv "SQL_PASSWORD"
  connectDatabase' <- getEnv "SQL_DATABASE"
  return $ defaultConnectInfo {
    connectHost = connectHost',
    connectPort = connectPort',
    connectUser = connectUser',
    connectPassword = connectPassword',
    connectDatabase = connectDatabase',
    connectSSL = Nothing
  }


installCacthCtrlC :: InitM ()
installCacthCtrlC = do
  mainThreadId <- threadId <$> get
  sqlConnection' <- asks sqlConnection
  sshTunnel' <- asks sshTunnel
  liftIO $ catchCtrlC mainThreadId $ do
    closeAll sshTunnel' sqlConnection'
    putStrLn "Bye"
  where
    catchCtrlC :: ThreadId -> IO () -> IO ()
    catchCtrlC mainThreadId handler = do
      _ <- installHandler keyboardSignal (Catch (handler >> killThread mainThreadId)) Nothing
      return ()

closeAll :: Maybe SshTunnel -> Maybe Connection -> IO ()
closeAll sshTunnel' sqlConnection' = do
  case sqlConnection' of
    Nothing -> return ()
    Just sc -> close sc
  case sshTunnel' of
    Nothing -> do
      filePath <- getEnvIO "SSH_TUNNELINFO"
      case filePath of
        Nothing -> return ()
        Just fp ->
          loadSshTunnel (decodeString fp) >>= closeSshTunnel
    Just st' -> closeSshTunnel st'


connectSql :: SshTunnel -> InitM ()
connectSql tunnelID = do
  sqlConnectResult <- sqlInfo >>= liftIO . try . connect
  case sqlConnectResult of
    Right sqlConnect -> do
      modify $ \s -> s { sqlConnection = Just sqlConnect }
    Left e -> do
      closeSshTunnel tunnelID
      throwError $ ConnectSqlFailed e

connectAll :: InitM ()
connectAll = do
  installCacthCtrlC
  connectTunnel >>= connectSql




















--
