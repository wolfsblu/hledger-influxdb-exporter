{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (loadFile, defaultConfig, onMissingFile)
import Control.Lens ( (&), (.~) )
import Data.Text (pack)
import Database.InfluxDB (writeBatch, writeParams)
import Exporter (toMeasurements)
import Hledger.Data.Types (jtxns)
import Hledger.Read (defaultJournal)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment.Blank (getEnvDefault, getEnvironment)
import qualified Database.InfluxDB.Types as I

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) (putStrLn "No .env file found, using defaults")
  dbName <- getEnvDefault "INFLUXDB_NAME" "finance"
  host <- getEnvDefault "INFLUXDB_HOST" "localhost"
  port <- getEnvDefault "INFLUXDB_PORT" "8086"
  ssl <- getEnvDefault "INFLUXDB_USE_SSL" "False"

  journal <- defaultJournal
  let measurements = toMeasurements (jtxns journal)
  let db = I.Database (pack dbName)
  let server = I.Server (pack host) (read port :: Int) (read ssl :: Bool)
  let params = writeParams db & I.server .~ server
                              & I.manager .~ Left tlsManagerSettings

  writeBatch params measurements
  putStrLn $ "Wrote " ++ show (length measurements) ++ " measurements."