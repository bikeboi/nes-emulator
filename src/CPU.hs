{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards #-}

module CPU (exec,stepCPU) where
import Prelude hiding (and)
import Debug.Trace

import Util
import CPU.Internal
import CPU.AddressMode
import CPU.OpCode
import CPU.Decode

import qualified Data.ByteString as B
import Data.Bits (testBit)
import Data.Word
import Data.Int
import Control.Monad
import Control.Monad.Except (liftEither)

-- Executing instructions
stepCPU :: CPU s ()
stepCPU = do i <- getIR
             case i of
               Just intrpt -> handleIR intrpt
               Nothing -> execNextOp
  where execNextOp = eat8 >>= exec . lookupCode

handleIR :: Interrupt -> CPU s ()
handleIR i = jumpTo (iLoc i)
  where iLoc IRQ = 0xFFFE
        iLoc RST = 0xFFFC
        iLoc NMI = 0xFFFA
        jumpTo a = do
          p <- getPC
          let (l,h) = (low p,high p)
          pushStack h >> pushStack l
          getPS >>= pushStack
          ind a >>= setPC

exec :: OpCode -> CPU s ()
exec (op,am) = do
  a <- fetchArg am
  case op of
    LDA -> lda =<< arg8 a
    LDX -> ldx =<< arg8 a
    LDY -> ldy =<< arg8 a
    STA -> sta =<< arg16 a
    STX -> stx =<< arg16 a
    STY -> sty =<< arg16 a
    TAX -> tax
    TAY -> tay
    TXA -> txa
    TYA -> tya
    TSX -> tsx
    TXS -> txs
    PHA -> pha
    PHP -> php
    PLA -> pla
    PLP -> plp
    AND -> and =<< arg8 a
    EOR -> eor =<< arg8 a
    ORA -> ora =<< arg8 a
    BIT -> bit =<< arg8 a
    ADC -> adc =<< arg8 a
    SBC -> sbc =<< arg8 a
    CMP -> cmp =<< arg8 a
    CPX -> cpx =<< arg8 a
    CPY -> cpy =<< arg8 a
    INC -> inc =<< arg16 a
    INX -> inx
    INY -> iny
    DEC -> dec =<< arg16 a
    DEX -> dex
    DEY -> dey
    ASL -> mutRef am op asl
    LSR -> mutRef am op lsr
    ROL -> mutRef am op rol
    ROR -> mutRef am op ror
    JMP -> jmp =<< arg16 a
    JSR -> jsr =<< arg16 a
    RTS -> rts
    BCC -> bcc =<< arg16 a
    BCS -> bcs =<< arg16 a
    BEQ -> beq =<< arg16 a
    BMI -> bmi =<< arg16 a
    BNE -> bne =<< arg16 a
    BPL -> bpl =<< arg16 a
    BVC -> bvc =<< arg16 a
    BVS -> bvs =<< arg16 a
    CLC -> clc
    CLI -> cli
    CLV -> clv
    CLD -> return () -- Do nothing
    SEC -> sec
    SEI -> sei
    SED -> return () -- Do nothing
    NOP -> return () -- Do nothing
    RTI -> rti
    BRK -> setIR IRQ
    o   -> cpuErr $ "Unrecognized opcode: " ++ show o

fetchArg :: AddrMode -> CPU s (Either Word16 Word8)
fetchArg Impl = return . Right $ 0x00 -- This will never be used anyway
fetchArg Acc  = Right <$> getA
fetchArg Imm  = Right <$> eat8
fetchArg Rel  = eat8 >>= rel . toS8 >>= return . Left
fetchArg x    = Left  <$> resolve x

arg8 :: Either Word16 Word8 -> CPU s Word8
arg8 (Left x)  = readRAM x
arg8 (Right x) = return x

arg16 :: Either Word16 Word8 -> CPU s Word16
arg16 (Right x) = cpuErr "How did you mess this up so bad m8"
arg16 (Left x)  = return x

pnt :: Word16 -> CPU s Word8
pnt addr = readRAM addr

resolve :: AddrMode -> CPU s Word16
resolve Z  = eat8 >>= zp
resolve Zx = eat8 >>= zpX
resolve Zy = eat8 >>= zpY
resolve A  = eat16 >>= ab
resolve Ax = eat16 >>= abX
resolve Ay = eat16 >>= abY
resolve Ind = eat16 >>= ind
resolve Imm = cpuErr "Cannot dereference an immediate value"
resolve XInd = eat8 >>= ixIn
resolve IndY = eat8 >>= inIx
resolve Impl = cpuErr "what"

-- Instructions that take both accumulator and memory as args
mutRef :: AddrMode -> Op -> (Word8 -> CPU s Word8) -> CPU s ()
mutRef Acc _ f = do ra <- getA
                    f ra >>= setA
mutRef Imm op _  = cpuErr $ concat ["Cannot mutate immediate address "
                                    ,show op," "
                                    ,show Imm]

mutRef Impl op _ = cpuErr $ "Cannot mutate implied address " ++ show op
mutRef am _ f = do a <- resolve am
                   val <- readRAM a
                   f val >>= writeRAM a
