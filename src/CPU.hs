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
exec (op,a) = case op of
    LDA -> pnt a >>= lda
    LDX -> pnt a >>= ldx
    LDY -> pnt a >>= ldy
    STA -> resolve a >>= sta
    STX -> resolve a >>= stx
    STY -> resolve a >>= sty
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
    AND -> pnt a >>= and
    EOR -> pnt a >>= eor
    ORA -> pnt a >>= ora
    BIT -> pnt a >>= bit
    ADC -> pnt a >>= adc
    SBC -> pnt a >>= sbc
    CMP -> pnt a >>= cmp
    CPX -> pnt a >>= cpx
    CPY -> pnt a >>= cpy
    INC -> resolve a >>= inc
    INX -> inx
    INY -> iny
    DEC -> resolve a >>= dec
    DEX -> dex
    DEY -> dey
    ASL -> mutRef a op asl
    LSR -> mutRef a op lsr
    ROL -> mutRef a op rol
    ROR -> mutRef a op ror
    JMP -> resolve a >>= jmp
    JSR -> resolve a >>= jsr
    RTS -> rts
    BCC -> resolveRel a >>= bcc
    BCS -> resolveRel a >>= bcs
    BEQ -> resolveRel a >>= beq
    BMI -> resolveRel a >>= bmi
    BNE -> resolveRel a >>= bne
    BPL -> resolveRel a >>= bpl
    BVC -> resolveRel a >>= bvc
    BVS -> resolveRel a >>= bvs
    CLC -> clc
    CLI -> cli
    CLV -> clv
    CLD -> return () -- Do nothing
    SEC -> sec
    SEI -> sei
    SED -> return () -- Do nothing
    NOP -> nop
    RTI -> rti
    BRK -> setIR IRQ

pnt :: AddrMode -> CPU s Word8
pnt Imm = eat8
pnt Acc = getA
pnt am = resolve am >>= readRAM

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
resolve Rel  = eat8 >>= rel . toS8
resolve Impl = cpuErr "what"

resolveRel :: AddrMode -> CPU s Int8
resolveRel Rel = toS8 <$> eat8
resolveRel _ = cpuErr "Cannot resolve relative address in that mode"

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
