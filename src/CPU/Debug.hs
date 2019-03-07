{-# LANGUAGE RankNTypes, OverloadedStrings, RecordWildCards, TupleSections, FlexibleContexts #-}

module CPU.Debug where

import Prelude hiding (and)

import qualified Data.ByteString as B
import Control.Monad.Writer.Lazy
import Control.Monad.Except (catchError,liftEither)
import Control.Monad.Trans.Class (lift)
import Data.Word

import Util
import Memory
import CPU.Decode
import CPU.OpCode
import CPU.Internal
import CPU.ROM
import CPU

-- Some tests
runNesTest x = runDebugger x 0xC000 nestestFile

-- MAIN
runDebugger :: Int -> Word16 -> FilePath -> IO ()
runDebugger to start file = do
  bytes <- B.readFile file
  mem <- initMem
  (Right (a,log),w) <- runCPU mem $ runCPULog
    $ do lift $ loadROM bytes >> setPC   start
         forM [0..to] $ const $ execWithLogging stepCPU
  mapM_ print log

nestestFile = "roms/rom-tests/nestest.nes"

-- Logging
type Log = [String]
type CPULog = WriterT Log CPU

runCPULog :: CPULog a -> CPU (a,[String])
runCPULog = runWriterT

execWithLogging :: CPU () -> CPULog ()
execWithLogging ca = pass $ do
  (a,x,y,s,p) <- lift $ liftM5 (,,,,) getA getX getY getSP getPC
  byte <- lift $ readRAM p
  op <- lift $ liftEither $ decodeOp byte
  val <- catchError (lift ca) logCPUErr
  return (val,(++ [logFmt (a,x,y) s p op]))
    where logCPUErr e = pass $ rP

type Registers = (Word8,Word8,Word8)
type StackPtr  = Word8
type ProgCntr  = Word16

logFmt :: Registers -> StackPtr -> ProgCntr -> OpCode -> String
logFmt (a,x,y) sp pc (op,addr) =
  concat $ map (++" ") $ [show16 pc
                         ,show op
                         ,"A:" ++ show16 a
                         ,"X:" ++ show16 x
                         ,"Y:" ++ show16 y
                         ,"SP:" ++ show16 sp
                         ,"---"
                         ,show addr]
