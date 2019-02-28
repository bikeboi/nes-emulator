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
lda,ldx,ldy :: Word8 -> CPU ()
lda x = negZero x >> setA x
ldx x = negZero x >> setX x
ldy x = negZero x >> setY x

-- Storing
sta,stx,sty :: Word16 -> CPU ()
sta a = regA >>= writeRAM a
stx a = regX >>= writeRAM a
sty a = regY >>= writeRAM a

-- Register Transfers
tax,tay,txa,tya,tsx,txs :: CPU ()
tax = regA >>= setX
tay = regA >>= setY
txa = regX >>= setA
tya = regY >>= setA
tsx = stack >>= setX
txs = regX >>= setStack

-- Stack operations
pha,php,pla,plp :: CPU ()
pha = regA >>= pushStack
php = status >>= pushStack
pla = popStack >>= setA
plp = popStack >>= setStatus

-- Logical operations
and,eor,ora,bit :: Word8 -> CPU ()
and x = logical (.&. x)
eor x = logical (xor x)
ora x = logical (.|. x)
bit x = do
  res <- regA >>= return . (.&. x)
  if testBit res 7 then setV else clrV 
  negZero res
  
-- Arithmetic
adc, sbc :: Word8 -> CPU ()
adc x = do a <- regA
           c <- (.&. 0x01) <$> status
           let ovf = ovfAdd x a .|. ovfAdd (x+a) c
               carries = carry a x || carry (x+a) 1
               res = x + a + c
           if ovf then setV else clrV
           if carries then setC else clrC
           negZero res >> setA res

sbc x = do a <- regA
           c <- complement . (.&. 0x01) <$> status
           let ovf = ovfSub x a || ovfSub (x-a) c
               carries = not ovf
               res = x - a - c
           if ovf then setV else clrV
           if carries then setC else clrC
           negZero res >> setA res

cmp, cpx, cpy :: Word8 -> CPU ()
cmp x = regA >>= comparison x
cpx x = regX >>= comparison x
cpy x = regY >>= comparison x

-- Increments
inc :: Word16 -> CPU ()
inc a = readRAM a >>= writeRAM a . (+1)
  
inx, iny :: CPU ()
inx = mutX (+1)
iny = mutY (+1)

-- Decrements
dec :: Word16 -> CPU ()
dec a = readRAM a >>= writeRAM a . (+ (negate 1))

dex, dey :: CPU ()
dex = mutX (\x -> to8 $ toS8 x + (negate 1))
dey = mutY (\y -> to8 $ toS8 y + (negate 1))

-- Logical ops
-- THESE RETURN VALUES (annoyingly, since the accumulator can be used)
asl, lsr, rol, ror :: Word8 -> CPU Word8
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
jmp, jsr :: Word16 -> CPU ()
jmp a = mutProg (const a)

jsr a = do p <- (\i -> i - 1) <$> prog
           let hi = high p
               lo = low p
           pushStack hi
           pushStack lo
           mutProg (const a)

rts :: CPU ()
rts = do v <- lendian <$> popStack <*> popStack
         mutProg (const (v+1))

-- Branching
bcc, bcs, bne, beq, bpl, bmi, bvc, bvs :: Int8 -> CPU ()
bcc a = not <$> sC >>= branch a
bcs a = sC >>= branch a
bne a = not <$> sZ >>= branch a
beq a = sZ >>= branch a
bpl a = not <$> sN >>= branch a
bmi a = sN >>= branch a
bvc a = not <$> sV >>= branch a
bvs a = pure True >>= branch a

-- Clear flags
clc, cli, clv :: CPU ()
clc = clrC
cli = clrI
clv = clrV

-- Set flags
sec, sei :: CPU ()
sec = setC
sei = setI

-- Misc.
brk, nop, rti :: CPU ()
brk = error "End of program"
nop = return ()
rti = do popStack >>= setStatus
         a <- lendian <$> popStack <*> popStack
         mutProg (const a)

-- Helpers
negZero :: Word8 -> CPU ()
negZero x = do if x == 0 then setZ else clrZ
               if x < 0 then setN else clrN

comparison :: Word8 -> Word8 -> CPU ()
comparison x y = do
  if x >= y then setC else clrC
  if x == y then setZ else clrZ
  if x <  y then setN else clrN

branch :: Int8 -> Bool -> CPU ()
branch a b = if b
             then mutProg (to16 . (+ toS16 a) . toS16)
             else return ()

logical :: (Word8 -> Word8) -> CPU ()
logical f = do res <- regA >>= return . f
               negZero res >> setA res

ovfAdd :: Word8 -> Word8 -> Bool
ovfAdd x y = let calc = complement (x `xor` y) .&. (x `xor` (x+y)) .&. 0x80
             in calc == 0x80

ovfSub :: Word8 -> Word8 -> Bool
ovfSub x y = ovfAdd x (complement y)

carry :: Word8 -> Word8 -> Bool
carry x y = 255 - x < y
