module Init.Type where

import Control.Monad.State
import Control.Monad.Except
import qualified Control.SSH.Tunnel as Ssh
import qualified Database.MySQL.Simple as Sql
import Control.Concurrent (ThreadId)
import Control.Exception (SomeException)

data InitError =
    ParseFail String
  | GetEnvFailed String SomeException
  | ConnectSqlFailed SomeException deriving (Show)

data InitState = InitState {
    sshTunnel :: Maybe Ssh.SshTunnel
  , sqlConnection :: Maybe Sql.Connection
  , threadId :: ThreadId
}
type InitM = ExceptT InitError (StateT InitState IO)

runInit :: ThreadId -> InitM a -> IO ((Either InitError a), InitState)
runInit threadId' exec = runStateT (runExceptT exec) (InitState Nothing Nothing threadId')

asks :: (InitState -> a) -> InitM a
asks f = f <$> get
