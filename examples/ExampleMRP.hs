{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Psephology.Polling.MRP

import Control.Monad (mzero)
import GHC.Generics
import System.IO
import System.Exit (exitFailure)
import System.Random.MWC (createSystemRandom)
import Data.Random
import Data.List
import Data.Maybe
import Data.Either
import Data.ByteString (ByteString, hGetSome, empty)
import qualified Data.ByteString.Lazy as BL
import Data.Csv (FromNamedRecord(..), (.:), Header, decodeByName, runParser)
import qualified Data.Vector as V

main :: IO ()
main = do
    polling <- filterNonRespondents <$> importCSV "examples/mrp/cces18_common_vv.csv" :: IO [Participant]
    putStrLn $ "Imported " <> show (length polling) <> " participants"
    postStratCells <- importCSV "examples/mrp/poststrat_df.csv" :: IO [CellImport]
    putStrLn $ "Imported " <> show (length postStratCells) <> " poststratification cells"
    let states = nub $ map (_state . cdata) postStratCells 
    let polling' = map (updateStateLabel states) polling
    let cells' = mrpCells polling' postStratCells
    mwc <- createSystemRandom
    ps <- sampleFrom mwc (meanPS 200 1e-5 cells')
    print ps

filterNonRespondents :: [Participant] -> [Participant]
filterNonRespondents =
    filter isRespondent

importCSV :: FromNamedRecord a => String -> IO [a]
importCSV path = do
    csvFile <- BL.readFile path
    case decodeByName csvFile of
        Left err -> do putStrLn err; return []
        Right (h, v) -> return $ V.toList v


-- Decode input

data Participant = Participant Response CellData
data CellImport  = CellImport  Int      CellData

data Response
    = Support
    | Oppose
    | NoResponse

supportsQuestion :: Participant -> Bool
supportsQuestion (Participant Support _) = True
supportsQuestion _                       = False

isRespondent :: Participant -> Bool
isRespondent (Participant NoResponse _) = False
isRespondent _                          = True

cdata :: CellImport -> CellData
cdata (CellImport _ d) = d

updateStateLabel :: [String] -> Participant -> Participant
updateStateLabel states (Participant res cdata) = 
    Participant res
        (cdata { _state = states !! (read (_state cdata) - 1) })

data CellData = CellData
    { _state  :: String
    , _race   :: String
    , _gender :: Double
    , _age    :: String
    , _educ   :: String
    } deriving (Eq)

abortLabel :: String -> Response
abortLabel "1" = Support
abortLabel "2" = Oppose
abortLabel ""  = NoResponse

statLabel :: Int -> String
statLabel i = show $ i - numToSkip
    where numToSkip = length $ filter (< i) [3, 7, 11, 14, 43, 52]

raceLabel :: Int -> String
raceLabel 1 = "White"
raceLabel 2 = "Black"
raceLabel 3 = "Hispanic"
raceLabel _ = "Other" 

-- In gender, out isMale
gendLabel :: Int -> Double
gendLabel i = [-0.5, 0.5] !! (i - 1)

-- In birthyr, out age cohort
-- This study was done in 2018
ageLabel :: Int -> String
ageLabel birthyr
    | age <= 29 = "18-29"
    | age <= 39 = "30-39"
    | age <= 49 = "40-49"
    | age <= 59 = "50-59"
    | age <= 69 = "60-69"
    | age <= 120 = "70+"
    where age = 2018 - birthyr

educLabel :: Int -> String
educLabel 1 = "No HS"
educLabel 2 = "HS"
educLabel 3 = "Some college"
educLabel 4 = "Some college"
educLabel 5 = "4-year College"
educLabel 6 = "Post-grad"

instance FromNamedRecord Participant where
    parseNamedRecord v =
        Participant
            <$> abortLabel <$> v .: "CC18_321d"
            <*> (CellData
                <$> (statLabel <$> v .: "inputstate")
                <*> (raceLabel <$> v .: "race")
                <*> (gendLabel <$> v .: "gender")
                <*> (ageLabel  <$> v .: "birthyr")
                <*> (educLabel <$> v .: "educ"))


instance FromNamedRecord CellImport where
    parseNamedRecord v =
        CellImport 
            <$> v .: "n"
            <*> (CellData
                <$> v .: "state"
                <*> v .: "eth"
                <*> v .: "male"
                <*> v .: "age"
                <*> v .: "educ")

-- Tally

mrpCells :: [Participant] -> [CellImport] -> [Cell]
mrpCells participants cells =
    let cdatas  = map cdata cells
        states  = nub $ map _state cdatas
        races   = nub $ map _race   cdatas
        genders = nub $ map _gender cdatas
        ages    = nub $ map _age    cdatas
        educs   = nub $ map _educ   cdatas
     in map (\cell@(CellImport popN cdata) -> 
        let r = filter (inCell cell) participants 
            n = length r + 1
            y = length (filter supportsQuestion r)
         in
        Cell
            ((fromIntegral y + 0.5) / (fromIntegral n + 1))
            n
            popN
            [ fromMaybe (-1) $ elemIndex (_state cdata) states
            , fromMaybe (-1) $ elemIndex (_race cdata) races
            , fromMaybe (-1) $ elemIndex (_gender cdata) genders
            , fromMaybe (-1) $ elemIndex (_age cdata) ages
            , fromMaybe (-1) $ elemIndex (_educ cdata) educs
            ]
            ) cells

inCell :: CellImport -> Participant -> Bool
inCell cell (Participant _ d) = cdata cell == d
