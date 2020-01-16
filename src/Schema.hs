{-# LANGUAGE BangPatterns #-}
module Schema where


import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result (Result)


data SimpleResult = String

-- instance Result a => QueryResults (Only a) where
--     convertResults [fa] [va] = Only a
--         where !a = convert fa va
