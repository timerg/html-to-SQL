{-# LANGUAGE OverloadedStrings #-}
module Program where

import Connect
import Type
import Parser
import Control.Monad.Reader
import Control.Monad.Except
import qualified Database.MySQL.Simple as Sql



runProgram :: ProgramEnv -> ProgramM () -> IO ()
runProgram env program = do
  result <- runReaderT (runExceptT program) env
  case result of
    Left e -> print e
    Right () -> return ()


program :: ProgramEnv -> IO ()
program env = do
  putStrLn "input command"
  cmd <- getLine
  let command = parseInput cmd
  case command of
    Close ->
      closeAll (Just $ sshTunnel env) (Just $ sqlConnection env)
    Read -> do
      runProgram env $ do
        sqlConnect <- asks sqlConnection
        qResult <- liftIO $ Sql.query_ sqlConnect testQuery
        liftIO $ print (qResult :: [Sql.Only String])
      program env
    Unhandled -> do
      putStrLn "unhandled command"
      program env

testQuery :: Sql.Query
testQuery = "SELECT display_name FROM user"
