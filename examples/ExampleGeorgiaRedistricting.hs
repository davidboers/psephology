{-# LANGUAGE OverloadedStrings #-}

module ExampleGeorgiaRedistricting where

import Psephology.Redistricting.Utilitarian

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import Data.List (find, intercalate)

-- | Run the Georgia state-wide redistricting example and export results to CSV.
runGeorgiaRedistricting :: IO ()
runGeorgiaRedistricting = do
    jsonContent <- decodeFileStrict "test/redistricting/georgia.json"
    case jsonContent :: Maybe [Precinct] of
        Just precincts -> do
            let reduced = reduce 14 (precinctsToDistricts precincts)
            let equalized = equalize reduced
            writeFile "test/redistricting/georgia.csv" $ districtsCSV precincts equalized
        Nothing -> putStrLn "Failed to parse JSON."

instance FromJSON Precinct where
    parseJSON (Object v) = do
        meta <-
            Precinct
                <$> v .: "GEOID20"
                <*> v .: "TOTPOP"
        x <- v .: "INTPTLON20"
        y <- v .: "INTPTLAT20"
        -- z <- v .: "BVAP"
        -- tvap <- v .: "VAP"
        return $ meta [x, y]
    parseJSON _ = mzero