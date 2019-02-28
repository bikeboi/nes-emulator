{-# LANGUAGE RankNTypes #-}

module Memory (readMem,writeMem,loadChunk,snapshot,Memory,initMem) where

import Prelude hiding (replicate)

import qualified Data.Vector as V
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.STRef
import Data.Word
import Data.Bits
import qualified Data.ByteString as B

import Util

-- Memory mapping
type Memory = V.Vector (MVar Word8)

-- Initializing
initMem :: IO Memory
initMem = V.replicateM 0x10000 (newMVar 0x00)

-- Basic Memory primitives
readMem :: Memory -> Word16 -> IO Word8
readMem mem a = do m <- return $ mem V.! (fromIntegral . mirror) a
                   readMVar m

writeMem :: Memory -> Word16 -> Word8 -> IO ()
writeMem mem a x = do m <- return $ mem V.! (fromIntegral . mirror) a
                      void $ swapMVar m x

snapshot :: Memory -> IO (V.Vector Word8)
snapshot mem = mapM readMVar mem

-- Helpful combinators
loadChunk :: B.ByteString -> Word16 -> Memory -> IO ()
loadChunk b start mem = let bytes = V.fromList $ B.unpack b
                            (x,xs) = V.splitAt (fromIntegral start) mem
                        in V.zipWithM_ swapMVar xs bytes

-- Mirroring (nice and pure)
mirror :: Word16 -> Word16
mirror = mirrRam . mirrIO

-- First mirrored section of RAM
mirrRam :: Word16 -> Word16
mirrRam x = if not (x `within` (0x0800, 0x2000))
            then x
            else minimum . map abs . filter (>=0)
                 $ [x - 0x0800  -- Section 1: 0x0800 - 0x0FFF
                   ,x - 0x1000  -- Section 2: 0x1000 - 0x17FF
                   ,x - 0x1800] -- Section 3: 0x1800 - 0x1FFF

-- Second mirrored section of RAM
-- 0x2000 - 0x2007 mirrored every 8 bytes from:
--   0x2008 - 0x4000
mirrIO :: Word16 -> Word16
mirrIO x = if not (x `within` (0x2008,0x4000))
           then x
           else x - (x .&. 0xFFF0)

-- Helper
-- Non-inclusive range-check
within :: Word16 -> (Word16,Word16) -> Bool
within x (mx,mn) = x >= mn && x < mx 
  
