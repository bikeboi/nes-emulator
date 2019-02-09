module Main where

import Test.Hspec
import CPU

main = do
  memorySpec

-- CPU Tests
-- Memory Access
-- Memory Helper
runOp' = fmap fst . (>>=) initCPU . runOp

memorySpec = hspec $ do
  getSet
  zeroPageWrap

getSet = do
  describe "memory access and mutation" $ do
    it "stores 5 in memory location 12 and retrieves it" $ do
      runOp' getSetZ `shouldReturn` (0,5)
      
    it "stores 69 in memory location 345 and retrieves it" $ do
      runOp' getSetAddr `shouldReturn` (0,69)
  where getSetZ = do a <- getAddrZ 12
                     setAddrZ 12 5
                     getAddrZ 12 >>= return . (,) a
        getSetAddr = do a <- getAddr 345
                        setAddr 345 69
                        getAddr 345 >>= return . (,) a

zeroPageWrap = do
  describe "zero page addressing should wrap" $ do
    it "checks wrapping for Z,X addressing" $ do
      flip shouldReturn 12 $ runOp' $ setup X >> derefZ ZX 255
    it "checks wrapping for Z,Y addressing" $ do
      flip shouldReturn 12 $ runOp' $ setup Y >> derefZ ZY 255
  where fillReg reg = setReg reg 3
        fillMem = setAddrZ 2 12
        setup reg = fillReg reg >> fillMem
