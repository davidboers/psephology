{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Fraud where

import Psephology.Fraud

import Data.Csv
import Data.IORef
import qualified Data.Text.Encoding as TE

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Test.Tasty
import Test.Tasty.HUnit

testFraud :: IO [Return] -> TestTree
testFraud russia2024 =
    testGroup "Fraud"
        [ testCase ">100%; x" $ russia2024 >>= \returns ->
            length <$> xOver100 returns @?= Nothing
        , testCase ">100%; t" $ russia2024 >>= \returns ->
            length <$> tOver100 returns @?= Nothing
        , testCase ">100%; xt" $ russia2024 >>= \returns ->
            length <$> xOver100R returns @?= Nothing
        ]

acquire :: IO [Return]
acquire = do
    csvFile <- BL.readFile "test/Fraud/2024.csv"
    case decodeByName csvFile of
        Left err -> do putStrLn err; return []
        Right (_, v) -> return $ V.toList v

acquireRussia2024 :: IORef [Return] -> IO [Return]
acquireRussia2024 ref = do
    r <- acquire
    printFraudMsgs r
    writeIORef ref r
    return r

printFraudMsgs :: [Return] -> IO ()
printFraudMsgs =
    putStrLn . detectFraud 

protectedInt :: String -> Int
protectedInt "" = 0
protectedInt s  = read s

instance FromNamedRecord Return where
    parseNamedRecord v =
        Return
            <$> (protectedInt <$> v .: TE.encodeUtf8 "Путин Владимир Владимирович") -- Putin Vladimir Vladimirovich
            <*> (protectedInt <$> v .: TE.encodeUtf8 "Число действительных избирательных бюллетеней") -- "No. valid ballots"
            <*> (protectedInt <$> v .: TE.encodeUtf8 "Число избирательных бюллетеней, полученных участковой избирательной комиссией") -- No. voters included in voter list