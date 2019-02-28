{-# LANGUAGE RankNTypes #-}

module Memory (readMem,writeMem,Memory,initMem,MemMonad) where

import Data.Array.MArray
import Data.Array.IO
import Data.Word
import Data.Bits
import Util

-- Memory mapping
type Memory = IOArray Word16 Word8
type MemMonad = IO

-- Initializing
-- newArray :: m (a i e)
initMem :: MemMonad Memory
initMem = newArray (0x0000,0xFFFF) 0x00

-- Basic Memory primitives
readMem :: Memory -> Word16 -> MemMonad Word8
readMem mem x = readArray mem $ mirror x

writeMem :: Memory -> Word16 -> Word8 -> MemMonad ()
writeMem mem a x = writeArray mem (mirror a) x

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
  
