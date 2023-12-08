{-# LANGUAGE OverloadedStrings #-}

module Exporter (toMeasurements) where

import Data.Function (on)
import Data.List (inits, groupBy, mapAccumL, nub)
import qualified Data.Map as M
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime(..))
import Database.InfluxDB as I
import Database.InfluxDB.Types as I
import qualified Database.InfluxDB.Format as F
import Hledger.Data.Types as H
import Hledger.Read as H

toMeasurements :: [H.Transaction] -> [I.Line UTCTime]
toMeasurements = map toInflux

toInflux :: H.Transaction -> I.Line UTCTime
toInflux txn = Line "delta" tags fields (Just time) where
  time   = UTCTime (H.tdate txn) 0
  tags   = M.singleton "description" (toKey (fixup (H.tdescription txn)))
  fields = fmap I.FieldFloat (toDeltas txn)

toKey :: T.Text -> I.Key
toKey "" = "None" :: I.Key
toKey text = F.formatKey F.text text

fixup :: T.Text -> T.Text
fixup = T.replace "," "_" . T.replace " " "_"


toDeltas :: H.Transaction -> M.Map I.Key Double
toDeltas txn =
    let postings = concatMap explodeAccount (H.tpostings txn)
        accounts = nub (map H.paccount postings)
    in M.fromList [ (fromString (T.unpack a), val)
                  | a <- accounts
                  , let ps  = filter ((==a) . H.paccount) postings
                  , let val = sum (map (value . H.pamount) ps)
                  ]

explodeAccount :: H.Posting -> [H.Posting]
explodeAccount p =
  [ p { H.paccount = a }
  | a <- tail . map (T.intercalate ":") . inits . T.splitOn ":" $ H.paccount p
  ]

value :: H.MixedAmount -> Double
value (H.Mixed amounts) = sum (map go (M.elems amounts)) where
  go (H.Amount "\915\233\188" q _ _) = fromRational (toRational q)
  go (H.Amount _ _ _ (Just (H.TotalPrice a))) = go a
  go (H.Amount {}) = 0