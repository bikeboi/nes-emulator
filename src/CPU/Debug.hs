{-# LANGUAGE RankNTypes, OverloadedStrings, RecordWildCards, TupleSections #-}

module CPU.Debug where

import Prelude hiding (and)

import Control.Monad.Writer.Lazy
import Control.Monad.Trans.Class (lift)
import Data.Word (Word16)
import Data.List

import Util
import CPU.Decode
import Memory
import CPU.Internal
import CPU.OpCode
import CPU

-- MAIN
debug = do (s,p) <- runDebug nestest
           case s of
             Left _ -> error "Catch exceptions in the logger"
             Right (_,log) ->
               writeFile "/home/bikeboi/Wazz/nes/nes-hs/logs/nestest.log"
               $ concat $ intersperse "\n" log

-- TEST SPECIFICATIONS
data Debug =
  Debug { _pcStart :: Word16
        , _testFile :: FilePath }

runDebug :: Debug -> IO (Either String ((),[String]), Processor)
runDebug Debug{..} = do bytes <- readROM _testFile
                        runCPULog $ do
                          lift $ loadROM bytes >> setProg _pcStart
                          runWithLogging
-- TESTS
nestestFile = "roms/rom-tests/nestest.nes"
blaargtestFile = "/home/bikeboi/Wazz/nes/nes-hs/roms/rom-tests/blaarg_cpu.nes"

nestest :: Debug
nestest = Debug 0xC000 nestestFile

blaarg :: Debug
blaarg = Debug 0x8000 blaargtestFile

-- DEBUGGER TYPES
data Exec = Halt | Continue | Interrupt deriving (Eq, Show)
type CPULog = WriterT [String] CPU

runCPULog :: CPULog a -> IO (Either String (a,[String]),Processor)
runCPULog x = do mem <- initMem
                 runCPU mem . runWriterT $ x

runWithLogging :: CPULog ()
runWithLogging = do
  p <- lift $ procc
  res <- stepCPU
  case res of
    Halt -> return ()
    Continue -> runWithLogging

-- Clean this up please, ffs
stepCPU :: CPULog Exec
stepCPU = pass $ do
  pc <- lift prog
  pro <- lift procc
  let appendLog = (++) . (:[])
  inst <- lift eat8
  let decoded = decodeOp inst
  case decoded of
    Left e        -> return (Halt,appendLog $ show16 pc ++ "\n" ++ show e)
    Right op@(BRK,_) -> return (Halt,appendLog $ logOp pc op)
    Right op@(o,a)   -> do lift $ exec op
                           return (Continue,appendLog $ logOp pc op ++ show pro)

logOp :: Word16 -> OpCode -> String
logOp pc op = (show16 pc) ++ " " ++ show op
