{-# LANGUAGE RecordWildCards, RankNTypes #-}

module Main where


import Test.Hspec

import CPU
import CPU.Processor (Processor(..),Cpu,runCpu,procc,readProg)
import Decode (Op(BRK),decodeOp)
import qualified Data.ByteString as B

import Control.Monad.Writer
import Control.Monad.Trans.Class

main = nestest

type CPUTest a = WriterT [String] Cpu a
runCPUTest :: CPUTest a -> IO (a,[String])
runCPUTest test = fmap fst $ runCpu $ runWriterT test

execCPUTest :: B.ByteString -> CPUTest ()
execCPUTest bs = runCPU
  where runCPU :: CPUTest()
        runCPU = do
          op <- lift $ readProg >>= return . decodeOp
          case op of
            (BRK,_) -> return ()
            _ -> do lift (exec op)
                    p <- lift procc
                    tell $ [show p]
                    runCPU

nestest :: IO ()
nestest = do bytes <- readROM "roms/nestest.nes"
             (_,log) <- runCPUTest $ execCPUTest bytes
             mapM_ print log
