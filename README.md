# Hledger InfluxDB Exporter

This little tool exports all the transactions of a local hledger journal 
to an arbitrary InfluxDB instance. The source code is an adapted version of 
the contents presented in [this article](https://memo.barrucadu.co.uk/hledger-influxdb-grafana.html)
by Michael Walker.

## Requirements

* A Haskell development environment
   * cabal `3.10.1.0`
   * GHC `9.6.3`
   * GHCup `0.1.20.0`
* An InfluxDB (version `1.x`) server 
* A local hledger journal file
* A `LEDGER_FILE` environment variable pointing to the journal

## Limitations

* Currently this tool only handles journals using `€` as currency. However, this may be changed by replacing `\915\233\188` in `Exporter.hs` with your desired currency

## Setup

Make sure you have Haskell installed, preferrably using [GHCup](https://www.haskell.org/ghcup/).

1. Navigate to the project directory in your terminal
1. Run `cabal build`
1. Copy `.env.example` to the build output directory   
   ```
   cp .env.example ./dist-newstyle/build/x86_64-windows/ghc-9.6.3/hledger-influxdb-exporter-0.1.0.0/x/hledger-influxdb-exporter/build/hledger-influxdb-exporter/.env
   ```
1. Modify the `.env` file to point to your InfluxDB server
1. Run the application

## Tasks

* [ ] Make currency configurable
* [ ] Properly handle transactions without description
* [ ] Be able to deal with multi currency journals