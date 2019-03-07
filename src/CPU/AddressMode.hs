module CPU.AddressMode where

import Util
import Data.Word
import Data.Int
import Data.Bits
import CPU.Internal

-- Addressing Modes
zp :: Word8 -> CPU s Word16
zp = return . to16

zpIx :: Word8 -> Word8 -> CPU s Word16
zpIx x ix = return . to16 $ x + ix

zpX, zpY :: Word8 -> CPU s Word16
zpX x = getX >>= zpIx x
zpY x = getY >>= zpIx x

rel :: Int8 -> CPU s Word16
rel a = do p <- getPC
           let lo = toS8 p + a
           return $ (p .&. 0xFF00) + to16 lo

ab :: Word16 -> CPU s Word16
ab = return

abIx :: Word16 -> Word8 -> CPU s Word16
abIx a ix = return $ a + (to16 ix)

abX, abY :: Word16 -> CPU s Word16
abX x = getX >>= abIx x
abY x = getY >>= abIx x

ind :: Word16 -> CPU s Word16
ind a = do let a' = (a .&. 0xFF00) + to16 (low a + 1)
           b1 <- readRAM a
           b2 <- readRAM a'
           return $ lendian b1 b2

ixIn :: Word8 -> CPU s Word16
ixIn a = do x <- getX
            ind' . to16 $ a + x
  where ind' addr = do b1 <- readRAM addr
                       b2 <- readRAM $ addr+1
                       return $ lendian b1 b2

inIx :: Word8 -> CPU s Word16
inIx a = ind (to16 a) >>= \aa -> fmap (\r -> to16 r + aa) getY
