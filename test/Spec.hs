{-# LANGUAGE RecordWildCards, RankNTypes #-}

module Main where


import Test.Hspec

import CPU.Decode
import CPU.Debug
import Control.Monad (zipWithM)

main = do
  hspec allOpCodes
  nestest

-- OpCode decoding tests
allOpCodes = do
  describe "OpCode Decoding" $ do
    it "checks if all opcodes can be decoded" $ do
      length (map lookupCode [0x00 .. 0xFF]) `shouldBe` (256 :: Int)

-- Full ROM Tests
nestest :: IO ()
nestest = do
  comp <- parseLogPC compareLog
  let compL = length comp
      spec = nestestLog compL
  out <- runDebugger spec
  let outL  = length out
  case compL == outL of
    False -> putStrLn $ "Logs are uneven. Expected:" ++ show compL ++ " Actual:" ++ show outL
    True -> putStrLn "They're the same length at least"
  writeLog "nestest" out

nestestLog :: Int -> LogSpec
nestestLog es =
  LogSpec { _romFile = (romPath ++ "tests/nestest.nes")
          , _pcStart = Just 0xC000
          , _epochs  = es
          , _logName = "nestest" }

compareLog :: FilePath
compareLog = "test/extra/nestest-success.log"

parseLogPC :: FilePath -> IO [String]
parseLogPC = fmap (map (head . words) . lines) . readFile
