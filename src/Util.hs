module Util where

import Data.Word
import Numeric
import Data.Bits
import Data.Int
import Data.Char (intToDigit)

to8 :: Integral a => a -> Word8
to8 = fromIntegral

to16 :: Integral a => a -> Word16
to16 = fromIntegral

toS8 :: Integral a => a -> Int8
toS8 = fromIntegral

toS16 :: Integral a => a -> Int16
toS16 = fromIntegral

show8 :: (Show a,Integral a) => a -> String
show8 x = showOct x ""

show16 :: (Show a,Integral a) => a -> String
show16 x = let s = showHex x ""
               len = length s
           in (if len < 2 then "0" else "") ++ s

show2 :: (Show a,Integral a) => a -> String
show2 x = let s = showIntAtBase 2 intToDigit x ""
              sl = length s
          in (if sl < 8 then (replicate (8-sl) '0') else "") ++ s

showBig16 :: Word16 -> String
showBig16 x = let hi = to8 $ (x .&. 0xFF00) .>>. 8
                  lo = to8 x
              in show16 hi ++ show16 lo

-- Convenience
infixl 6 .>>.
(.>>.) :: (Bits a, Integral a) => a -> Int -> a
x .>>. y = shiftR x y

infixl 6 .<<.
(.<<.) :: (Bits a, Integral a) => a -> Int -> a
(.<<.) x y = shiftL x y

-- Bit stuff
cbit :: Word8 -> Int -> Bool
cbit = testBit

-- Edianness
lendian :: Word8 -> Word8 -> Word16
lendian lo hi = ((to16 hi) .<<. 8) + to16 lo

-- Splitting 16-bit
low :: Word16 -> Word8
low = to8

high :: Word16 -> Word8
high x = to8 $ (x .&. 0xFF00) .>>. 8

