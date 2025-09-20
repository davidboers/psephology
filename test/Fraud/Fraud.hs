{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Fraud where

-- Unused for now
import Psephology.Fraud

import Test.Tasty

testFraud :: TestTree
testFraud =
    testGroup "Fraud"
        []

-- Run with `ghc -cpp -optP-DINDEPENDENT Fraud.hs`
#ifdef INDEPENDENT
main :: IO ()
main = putStrLn $ detectFraud returns
#endif