module Type where

import qualified Database.MySQL.Simple as Sql
import qualified Control.SSH.Tunnel as Ssh
import Control.Monad.Reader
import Control.Monad.Except
import Control.Exception (SomeException)



--
data Commands = Read | Close | Unhandled deriving (Show, Eq)

data ProgramEnv = ProgramEnv {
    sshTunnel :: Ssh.SshTunnel
  , sqlConnection :: Sql.Connection
}
data ProgramError = UnhandledProgramError deriving (Show, Eq)
type ProgramM = ExceptT ProgramError (ReaderT ProgramEnv IO)
