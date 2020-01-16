{-# LANGUAGE OverloadedStrings #-}
module Env where

-- import Filesystem.Path.CurrentOS (decodeString)
import LoadEnv

defaultEnvPath :: FilePath
defaultEnvPath = "./env/env-develope"

useEnv :: Maybe String -> IO ()
useEnv path = case path of
  Nothing -> loadEnvFrom defaultEnvPath
  Just s -> loadEnvFrom $ s
