module Parser where

import Type
import Init.Type
import Data.Attoparsec.Text.Lazy
import qualified System.Environment as Sys
import Control.Monad.Except
import qualified Control.Exception as Exception
import Data.Scientific (toBoundedInteger)
import Data.Text



getEnvIO :: String -> IO (Maybe String)
getEnvIO key = do
  result <- Exception.try $ Sys.getEnv key
  case result of
    Left e -> do
      print (e :: Exception.SomeException)
      return $ Nothing
    Right i -> return $ Just i


getEnv :: String -> InitM String
getEnv key = do
  result <- liftIO $ Exception.try $ Sys.getEnv key
  case result of
    Left e -> throwError $ GetEnvFailed key e
    Right i -> return i

getIntEnv :: String -> InitM Int
getIntEnv key = do
  s <- getEnv key
  case parseOnly (scientific <* endOfInput) (pack s) of
    Left _ -> throwError $ ParseFail ("parse " ++ key ++  " failed")
    Right sci ->
      case toBoundedInteger sci of
        Nothing -> throwError $ ParseFail (key ++ "is too large to parse")
        Just i -> return i


parseInput :: String -> Commands
parseInput str = case str of
  "Close" -> Close
  "Read" -> Read
  _ -> Unhandled
