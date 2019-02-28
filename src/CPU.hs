{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards #-}

module CPU (CPU,runCPU,exec,loadROM,readROM) where

import Prelude hiding (and)
import Debug.Trace

import Util           
import CPU.Internal
import CPU.OpCode
import CPU.Decode

import qualified Data.ByteString.Lazy as B
import Data.Bits (testBit)
import Data.Word
import Data.Int
import Control.Monad
import Control.Monad.Except (throwError)

-- Main functions
-- Loads ROM in and sets program counter (Takes ROM bytestring - header)
loadROM :: B.ByteString -> CPU ()
loadROM bs = let header = B.take 16 bs
                 body   = B.drop 16 bs
                 spec   = romSpec header
             in loadROM' body spec
  where limBool b x = if b then x else 0
        bankSize = 16 * 1024
        loadROM' bs ROMSpec{..} =
          let start = B.drop (limBool _trainer 512) bs -- We're ignoring trainers
              (lowerBank,rest) = B.splitAt bankSize start
              (upperBank,rest') = B.splitAt (limBool (_prgSize > 1) bankSize) rest
              prgRom = B.unpack $ lowerBank `mappend` upperBank
          in zipWithM_ writeRAM [0xC000..] prgRom

readROM :: FilePath -> IO B.ByteString
readROM fp = do f <- B.readFile fp
                let constant = B.unpack $ B.take 4 f
                if constant /= [0x4e,0x45,0x53,0x1a]
                  then error $ "Nasty ROM:" ++ show constant
                  else putStrLn ("READ ROM: " ++ fp) >> return f

romSpec :: B.ByteString -> ROMSpec
romSpec bs = let prgSize = B.index bs 4
                 chrSize = B.index bs 5
                 flag6 = B.index bs 2
                 trainer = testBit flag6 2
                 sram = testBit flag6 1
                 -- More Data Later
             in ROMSpec prgSize chrSize trainer sram

data ROMSpec =
  ROMSpec { _prgSize :: Word8
          , _chrSize :: Word8
          , _trainer :: Bool
          , _sram    :: Bool     -- More Data Later
          } deriving (Eq, Show)

-- Executing instructions
exec :: OpCode -> CPU ()
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
    BRK -> throwError "Handle BRK higher up mate"

pnt :: AddrMode -> CPU Word8
pnt Imm = eat8
pnt Acc = regA
pnt am = resolve am >>= readRAM

resolve :: AddrMode -> CPU Word16
resolve Z  = eat8 >>= zp
resolve Zx = eat8 >>= zpX
resolve Zy = eat8 >>= zpY
resolve A  = eat16 >>= ab
resolve Ax = eat16 >>= abX
resolve Ay = eat16 >>= abY
resolve Ind = eat16 >>= ind
resolve Imm = throwError "Cannot dereference an immediate value"
resolve XInd = eat8 >>= ixIn
resolve IndY = eat8 >>= inIx
resolve Rel  = eat8 >>= rel . toS8

resolveRel :: AddrMode -> CPU Int8
resolveRel Rel = toS8 <$> eat8
resolveRel _ = throwError "Cannot resolve relative address in that mode"

-- Instructions that take both accumulator and memory as args
mutRef :: AddrMode -> Op -> (Word8 -> CPU Word8) -> CPU ()
mutRef Acc _ f = do ra <- regA
                    f ra >>= setA
mutRef Imm op _  = throwError $ "Cannot mutate immediate address " ++ show op ++ " " ++ show Imm
mutRef Impl op _ = throwError $ "Cannot mutate implied address " ++ show op
mutRef am _ f = do a <- resolve am
                   val <- readRAM a
                   f val >>= writeRAM a

