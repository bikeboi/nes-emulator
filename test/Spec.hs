{-# LANGUAGE RecordWildCards, RankNTypes #-}

module Main where


import Test.Hspec

import CPU.Decode

main = hspec $ do
  allOpCodes

-- OpCode decoding tests
allOpCodes = do
  describe "OpCode Decoding" $ do
    it "checks if all opcodes can be decoded" $ do
      length (map lookupCode [0x00 .. 0xFF]) `shouldBe` (256 :: Int)
