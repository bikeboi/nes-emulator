{-# LANGUAGE BinaryLiterals, RankNTypes #-}

module CPU.OpCode (
    lda, ldx, ldy, sta, stx, sty
  , tax, tay, txa, tya, tsx, txs
  , pha, php, pla, plp
  , and, eor, ora, bit
  , adc, sbc, cmp, cpx, cpy
  , inc, inx, iny, dec, dex, dey
  , asl, lsr, rol, ror
  , jmp, jsr, rts
  , bcc, bcs, beq, bmi, bne, bpl, bvc, bvs
  , clc, cli, clv, sec, sei
  , brk, nop, rti) where

import Prelude hiding (and)

import CPU.Internal
import Util
import Data.Bits hiding (bit)
import Data.Word
import Data.Int
import Control.Monad (join)

-- Loading
lda,ldx,ldy :: Word8 -> CPU s ()
lda x = negZero x >> setA x
ldx x = negZero x >> setX x
ldy x = negZero x >> setY x

-- Storing
sta,stx,sty :: Word16 -> CPU s ()
sta a = getA >>= writeRAM a
stx a = getX >>= writeRAM a
sty a = getY >>= writeRAM a

-- Register Transfers
tax,tay,txa,tya,tsx,txs :: CPU s ()
tax = getA >>= setX
tay = getA >>= setY
txa = getX >>= setA
tya = getY >>= setA
tsx = getSP >>= setX
txs = getX >>= setSP

-- Stack operations
pha,php,pla,plp :: CPU s ()
pha = getA >>= pushStack
php = getPS >>= pushStack
pla = popStack >>= setA
plp = popStack >>= setPS

-- Logical operations
and,eor,ora,bit :: Word8 -> CPU s ()
and x = logical (.&. x)
eor x = logical (xor x)
ora x = logical (.|. x)
bit x = do
  res <- getA >>= return . (.&. x)
  if testBit res 7 then setV else clrV
  negZero res

-- Arithmetic
adc, sbc :: Word8 -> CPU s ()
adc x = do a <- getA
           c <- (.&. 0x01) <$> getPS
           let ovf = ovfAdd x a .|. ovfAdd (x+a) c
               carries = carry a x || carry (x+a) 1
               res = x + a + c
           if ovf then setV else clrV
           if carries then setC else clrC
           negZero res >> setA res

sbc x = do a <- getA
           c <- complement . (.&. 0x01) <$> getPS
           let ovf = ovfSub x a || ovfSub (x-a) c
               carries = not ovf
               res = x - a - c
           if ovf then setV else clrV
           if carries then setC else clrC
           negZero res >> setA res

cmp, cpx, cpy :: Word8 -> CPU s ()
cmp x = getA >>= comparison x
cpx x = getX >>= comparison x
cpy x = getY >>= comparison x

-- Increments
inc :: Word16 -> CPU s ()
inc a = readRAM a >>= writeRAM a . (+1)

inx, iny :: CPU s ()
inx = mutX (+1)
iny = mutY (+1)

-- Decrements
dec :: Word16 -> CPU s ()
dec a = readRAM a >>= writeRAM a . (+ (negate 1))

dex, dey :: CPU s ()
dex = mutX (\x -> to8 $ toS8 x + (negate 1))
dey = mutY (\y -> to8 $ toS8 y + (negate 1))

-- Logical ops
-- THESE RETURN VALUES (annoyingly, since the accumulator can be used)
asl, lsr, rol, ror :: Word8 -> CPU s Word8
asl x = let shifted = x .<<. 1
        in do negZero shifted
              if testBit x 7 then setC else clrC
              return shifted

lsr x = let shifted = x .>>. 1
        in do negZero shifted
              if testBit x 7 then setC else clrC
              return shifted

rol x = let roted = x .<<. 1
        in do c <- sC
              negZero roted
              if testBit x 7 then setC else clrC
              return $ if c then roted .|. 0x01 else roted

ror x = let roted = x .>>. 1
        in do c <- sC
              negZero roted
              if testBit x 0 then setC else clrC
              return $ if c then roted .|. 0x80 else roted

-- Jumps
jmp, jsr :: Word16 -> CPU s ()
jmp a = mutPC (const a)

jsr a = do p <- (\i -> i - 1) <$> getPC
           let hi = high p
               lo = low p
           pushStack hi
           pushStack lo
           mutPC (const a)

rts :: CPU s ()
rts = do v <- lendian <$> popStack <*> popStack
         mutPC (const (v+1))

-- Branching
bcc, bcs, bne, beq, bpl, bmi, bvc, bvs :: Int8 -> CPU s ()
bcc a = not <$> sC >>= branch a
bcs a = sC >>= branch a
bne a = not <$> sZ >>= branch a
beq a = sZ >>= branch a
bpl a = not <$> sN >>= branch a
bmi a = sN >>= branch a
bvc a = not <$> sV >>= branch a
bvs a = pure True >>= branch a

-- Clear flags
clc, cli, clv :: CPU s ()
clc = clrC
cli = clrI
clv = clrV

-- Set flags
sec, sei :: CPU s ()
sec = setC
sei = setI

-- Misc.
brk, nop, rti :: CPU s ()
brk = error "End of getPCram"
nop = return ()
rti = do popStack >>= setPS
         a <- lendian <$> popStack <*> popStack
         mutPC (const a)

-- Helpers
negZero :: Word8 -> CPU s ()
negZero x = do if x == 0 then setZ else clrZ
               if x < 0 then setN else clrN

comparison :: Word8 -> Word8 -> CPU s ()
comparison x y = do
  if x >= y then setC else clrC
  if x == y then setZ else clrZ
  if x <  y then setN else clrN

branch :: Int8 -> Bool -> CPU s ()
branch a b = if b
             then mutPC (to16 . (+ toS16 a) . toS16)
             else return ()

logical :: (Word8 -> Word8) -> CPU s ()
logical f = do res <- getA >>= return . f
               negZero res >> setA res

ovfAdd :: Word8 -> Word8 -> Bool
ovfAdd x y = let calc = complement (x `xor` y) .&. (x `xor` (x+y)) .&. 0x80
             in calc == 0x80

ovfSub :: Word8 -> Word8 -> Bool
ovfSub x y = ovfAdd x (complement y)

carry :: Word8 -> Word8 -> Bool
carry x y = 255 - x < y
