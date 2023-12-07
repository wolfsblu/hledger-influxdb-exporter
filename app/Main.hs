{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Database.InfluxDB as I
import Database.InfluxDB.Types as I
import Hledger.Data.Types as H
import Hledger.Read as H
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Exporter (toMeasurements)

main :: IO ()
main = do
  journal <- H.defaultJournal
  let measurements = toMeasurements (H.jtxns journal)
  let db = "finance"
  let server = I.Server "localhost" 443 True
  let p = I.writeParams db & I.server .~ server
                           & I.manager .~ Left tlsManagerSettings
  I.writeBatch p measurements
  putStrLn $ "Wrote " ++ show (length measurements) ++ " measurements."
