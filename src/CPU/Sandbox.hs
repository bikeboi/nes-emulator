{-# LANGUAGE BinaryLiterals, BangPatterns #-}

module CPU.Sandbox where

import qualified Data.Map.Strict as M
import Data.Array
import Data.Word
import Data.Bits ((.&.))
import CPU.Decode
import Lens.Micro.Platform

-- Main one
lookupCode :: Word8 -> (Op,AddrMode)
lookupCode = (,) <$> lookupOp <*> lookupAddr

-- Lookups
lookupOp :: Word8 -> Op
lookupOp = lookup' opMap

lookupAddr :: Word8 -> AddrMode
lookupAddr = lookup' addrMap

-- General lookup
lookup' :: M.Map Word8 (Array Word8 a) -> Word8 -> a
lookup' mp b = let arr = mp M.! (b .&. 3)
               in arr ! b
-- MAPS
addrMap = mkMap [am0,am1,am2,am3]
opMap   = mkMap [op0,op1,op2,op3]

mkMap :: [Matrix a] -> M.Map Word8 (Array Word8 a)
mkMap m = M.fromList $
          zipWith (\i a -> (i, matToArray a i)) [0 .. 3] m

matToArray :: Matrix a -> Word8 -> Array Word8 a
matToArray om start = array (0,255) $ concat $
  zipWith (\s r -> map (_1 %~ (+s)) r) [0x00, 0x20 .. 0xE0] $
  map (zip [start, start+4 .. start+28]) om


-- MATRICES
-- Address Matrices
type Matrix a = [[a]]

am0, am1, am2, am3 :: Matrix AddrMode
am0 =
  [[Impl,A,Impl,Impl] ++ replicate 4 Imm
  ,replicate 8 Z
  ,replicate 8 Rel
  ,replicate 8 Zx
  ,replicate 8 Impl
  ,replicate 8 Ax]

am1 = map (replicate 8) $
  [XInd, Z, Imm, A, IndY, Zx, Ay, Ay]

am2 =
  [replicate 4 Impl ++ replicate 4 Imm
  ,replicate 8 Z
  ,replicate 8 Impl
  ,replicate 8 A
  ,replicate 8 Impl
  ,replicate 4 Zx ++ replicate 2 Zy ++ [Zx,Zx]
  ,replicate 8 Impl
  ,replicate 4 Ax ++ [Ay,Ay] ++ replicate 2 Ax]

am3 = map (replicate 8) [XInd, Z, Imm, A, IndY]
      ++ [replicate 4 Zx ++ [Zy,Zy,Zx,Zx]]
      ++ [replicate 8 Ay]
      ++ [replicate 4 Ax ++ [Ay,Ay,Ax,Ax]]

-- Operation Matrixes
op0, op1, op2, op3 :: Matrix Op
op0 =
  [[BRK, NOP, PHP, NOP, BPL, NOP, CLC, NOP]
  ,[JSR, BIT, PLP, BIT, BMI, NOP, SEC, NOP]
  ,[RTI, NOP, PHA, JMP, BVC, NOP, CLI, NOP]
  ,[RTS, NOP, PLA, JMP, BVS, NOP, SEI, NOP]
  ,[NOP, STY, DEY, STY, BCC, STY, TYA, SHY]
  ,[LDY, LDY, TAY, LDY, BCS, LDY, CLV, LDY]
  ,[CPY, CPY, INY, CPY, BNE, NOP, CLD, NOP]
  ,[CPX, CPX, INX, CPX, BEQ, NOP, SED, NOP]]

op1 =
  [replicate 8 ORA
  ,replicate 8 AND
  ,replicate 8 EOR
  ,replicate 8 ADC
  ,[STA, STA, NOP, STA, STA, STA, STA, STA]
  ,replicate 8 LDA
  ,replicate 8 CMP
  ,replicate 8 SBC]

op2 =
  [[STP, ASL, ASL, ASL, STP, ASL, NOP, ASL]
  ,[STP, ROL, ROL, ROL, STP, ROL, NOP, ROL]
  ,[STP, LSR, LSR, LSR, STP, LSR, NOP, LSR]
  ,[STP, ROR, ROR, ROR, STP, ROR, NOP, ROR]
  ,[NOP, STX, TXA, STX, STP, STX, TXS, SHX]
  ,[LDX, LDX, TAX, LDX, STP, LDX, TSX, LDX]
  ,[NOP, DEC, DEX, DEC, STP, DEC, NOP, DEC]
  ,[NOP, INC, NOP, INC, STP, INC, NOP, INC]]

op3 =
  [[SLO, SLO, ANC, SLO, SLO, SLO, SLO, SLO]
  ,[RLA, RLA, ANC, RLA, RLA, RLA, RLA, RLA]
  ,[SRE, SRE, ALR, SRE, SRE, SRE, SRE, SRE]
  ,[RRA, RRA, ARR, RRA, RRA, RRA, RRA, RRA]
  ,[SAX, SAX, XAA, SAX, AHX, SAX, TAS, AHX]
  ,[LAX, LAX, LAX, LAX, LAX, LAX, LAS, LAX]
  ,[DCP, DCP, AXS, DCP, DCP, DCP, DCP, DCP]
  ,[ISC, ISC, SBC, ISC, ISC, ISC, ISC, ISC]]
