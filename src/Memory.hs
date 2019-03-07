{-# LANGUAGE RankNTypes #-}

module Memory (readMem,writeMem,loadChunk,snapshot,Memory,mirror,newMem) where

import Prelude hiding (replicate,read,length)

import Data.Vector.Mutable
import qualified Data.Vector as V
import Control.Monad (void,zipWithM_)
import Control.Monad.ST
import Data.Word
import Data.Bits
import qualified Data.ByteString as B

import Util

-- GENERIC MEMORY TYPES AND COMBINATORS
-- Memory mapping
type Memory s = STVector s Word8

newMem :: Int -> ST s (Memory s)
newMem x = replicate x 0x00

-- Basic Memory primitives
readMem :: Memory s -> Word16 -> ST s Word8
readMem mem a = read mem $ fromIntegral a

writeMem :: Memory s -> Word16 -> Word8 -> ST s ()
writeMem mem a x = flip (write mem) x $ fromIntegral a

snapshot :: Memory s -> ST s (V.Vector Word8)
snapshot mem = V.freeze mem

-- COMBINATORS
loadChunk :: B.ByteString -> (Word16 -> Word16) -> Word16 -> Memory s -> ST s ()
loadChunk b f start mem =
  let maxSize = fromIntegral $ length mem - 1
  in zipWithM_ (writeMem mem) (map f [start .. maxSize]) $ B.unpack b

-- MIRRORING
type Range = (Word16,Word16)
type Step  = Word16

mirror :: Range -> Step -> Word16 -> Word16 -> Word16
mirror r@(l,h) s m v =
  case v `within` r of
    False -> v
    True -> v `mod` m+s

-- Non-inclusive range-check
within :: Word16 -> (Word16,Word16) -> Bool
within x (mx,mn) = x >= mn && x < mx
