{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}

module CPU.Debug where

import Prelude hiding (and)
import Data.Char (toUpper)

import qualified Data.ByteString as B
import Control.Monad.Writer.Lazy
import Control.Monad.Except (catchError,liftEither)
import Control.Monad.Trans.Class (lift)
import Data.Word
import Lens.Micro.Platform

import Util
import CPU.Decode
import CPU.OpCode
import CPU.Internal
import CPU.ROM
import CPU

-- Hardcoding those log file locations BOII
logPath,romPath :: FilePath
(romPath,logPath) = ("roms/","logs/")

-- MAIN
runDebugger :: LogSpec -> IO [String]
runDebugger LogSpec{..} = do
  bytes <- B.readFile _romFile
  (Right (_,log),_) <- return $ runCPU $ runCPULog $ do
    lift $ do
      loadROM bytes
      case _pcStart of
        Nothing -> return ()
        Just s  -> setPC s
    forM [1.._epochs] $ const $ execWithLogging stepCPU
  return log

writeLog :: String -> [String] -> IO ()
writeLog name = writeFile (logPath ++ name ++ ".log") . concat . map (++"\n")

readLog :: String -> IO [String]
readLog name = fmap lines $ readFile $ logPath ++ name ++ ".log"

--
data LogSpec =
  LogSpec { _romFile :: FilePath
          , _pcStart :: Maybe Word16
          , _epochs  :: Int
          , _logName :: String } deriving Show

-- Logging
type Log = [String]
type CPULog s = WriterT Log (CPU s)

runCPULog :: CPULog s a -> CPU s (a,[String])
runCPULog = runWriterT

execWithLogging :: CPU s () -> CPULog s ()
execWithLogging ca = pass $ do
  procc <- lift $ proccessor
  byte <- lift $ readRAM =<< getPC
  let op = lookupCode byte
  let log' = logFmt procc byte op
  val <- catchError (lift ca) $ logCPUErr
  return (val,(++ [log']))
    where logCPUErr e = pass $ return ((),(++[e]))

type Registers = (Word8,Word8,Word8)
type StackPtr  = Word8
type ProgCntr  = Word16

logFmt :: Processor -> Word8 -> OpCode -> String
logFmt p byte (op,addr) =
  concat $ map (++" ") $ [map toUpper $ show16 $ p ^. pPC
                         ,show op
                         ,show16 byte
                         ,"A:"  ++ show16 (p ^. pA)
                         ,"X:"  ++ show16 (p ^. pX)
                         ,"Y:"  ++ show16 (p ^. pY)
                         ,"SP:" ++ show16 (p ^. pSP)
                         ,"PS:" ++ show2  (p ^. pPS)
                         ,"---"
                         ,show addr]
