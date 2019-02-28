{-# LANGUAGE BinaryLiterals, FlexibleContexts #-}

module CPU.Decode (decodeOp
                  , show8, show16, show2
                  , AddrMode(..)
                  , Op(..)
                  , OpCode) where 

import Util
import Data.Word
import Data.Char (intToDigit)
import Data.Bits

import qualified Data.Map as M

-- Format for binary is: aaabbbcc
-- Octal number: abc

decodeOp :: Word8 -> Either String OpCode
decodeOp b = let oc = ocChunk b
             in case (oc,addrMode =<< oc,opCode =<< oc) of
                  (_,Just a,Just o) -> Right (o,a)
                  _                 -> Left $ "Illegal opcode " ++ show16 b

type OctalCode = (Word8,Word8,Word8)
type OpCode = (Op,AddrMode)
type Decode = Maybe OpCode

-- Extracting octal code
ocChunk :: Word8 -> Maybe OctalCode
ocChunk x = let a = (x .&. 0b11100000) .>>. 5
                b = (x .&. 0b00011100) .>>. 2
                c = x .&. 0b00000011
            in if a > 7 || b > 7 || c > 2
               then Nothing
               else Just (a,b,c)
-- Dereferencing
data AddrMode =
    Z | Zx | Zy
  | A | Ax | Ay
  | Ind
  | Imm
  | XInd | IndY
  | Impl
  | Rel
  | Acc
  deriving (Eq, Show)

data Op =
    LDA | LDX | LDY | STA | STX | STY       -- Loading and storage 
  | TAX | TAY | TXA | TYA | TSX | TXS       -- Regoster transfers
  | PHA | PHP | PLA | PLP                   -- Stack operayions
  | AND | EOR | ORA | BIT                   -- Logical operaionts
  | ADC | SBC | CMP | CPX | CPY             -- Arithmetic
  | INC | INX | INY | DEC | DEX | DEY       -- Increments and decrements
  | ASL | LSR | ROL | ROR                   -- Bit shifts
  | JMP | JSR | RTS                         -- Control flow
  | BCC | BCS | BEQ | BMI | BNE | BPL | BVC | BVS -- Branching
  | CLC | CLI | CLV | CLD | SEC | SEI | SED     -- Flag control
  | BRK | NOP | RTI
  deriving (Eq, Show, Ord)

-- Deciphering Address Mode
addrMode :: OctalCode -> Maybe AddrMode
-- Straightforward encodings
addrMode (_,1,_) = pure Z
addrMode (a,3,c) = if a == 0 && c == 0 then Nothing
                   else pure $ if a == 3
                               then Ind
                               else A
addrMode (_,5,_) = pure Zx
addrMode (_,7,_) = pure Ax
-- Dependent
addrMode (a,2,c) = col2 a c
  where col2 _ 0 = pure Impl
        col2 _ 1 = pure Imm
        col2 a 2 = if a > 3 then pure Impl else pure Acc

addrMode (_,4,c) = if c == 0 then pure Rel else pure IndY
addrMode (_,6,c) = if c == 0 || c == 2 then pure Impl else pure Ay

addrMode (a,0,c) = col0 a c
  where col0 _ 1 = pure XInd
        col0 a 2 = if a == 5 then pure Imm else illAddr
        col0 a 0 = if a > 3 then pure Imm
                   else if a == 1 then pure A else pure Impl

addrMode n = Nothing

-- Deciphering the OpCode
opCode :: OctalCode -> Maybe Op
opCode (a,_,1) = row1 a
  where row1 0 = pure ORA
        row1 1 = pure AND
        row1 2 = pure EOR
        row1 3 = pure ADC
        row1 4 = pure STA
        row1 5 = pure LDA
        row1 6 = pure CMP
        row1 7 = pure SBC
        row1 _ = Nothing

opCode (4,6,2) = pure TXS
opCode (5,6,2) = pure TSX

opCode (a,b,2) = row2 a b
  where row2 0 _ = pure ASL
        row2 1 _ = pure ROL
        row2 2 _ = pure LSR
        row2 3 _ = pure ROR
        row2 6 b = pure $ if b == 2 then DEX else DEC
        row2 7 b = pure $ if b == 2 then NOP else INC
        row2 4 b = if b `elem` [1,3,5] then pure STX
                   else if b == 2 then pure TXA
                        else if b == 6 then pure TXS else Nothing 
        row2 5 b = pure $ if b == 2 then TAX
                          else if b == 6 then TSX else LDX
        row2 _ _ = illOp

opCode (a,b,0) = (if even b then evenMap else oddMap) b (fromIntegral a)
  where evenMap :: Word8 -> Int -> Maybe Op
        evenMap 0 a = (map pure [BRK,JSR,RTI,RTS] ++ illOp:map pure [LDY,CPY,CPX]) !! a
        evenMap 2 a = map pure [PHP,PLP,PHA,PLA,DEY,TAY,INY,INX] !! a
        evenMap 4 a = map pure [BPL,BMI,BVC,BVS,BCC,BCS,BNE,BEQ] !! a
        evenMap 6 a = map pure [CLC,SEC,CLI,SEI,TYA,CLV,CLD,SED] !! a

        oddMap 1 a = [illOp,pure BIT,illOp,illOp,pure STY,pure LDY,pure CPY,pure CPX] !! a
        oddMap 3 a = (illOp : map pure [BIT,JMP,JMP,STY,LDY,CPY,CPX]) !! a
        oddMap 5 4 = pure STY
        oddMap 5 5 = pure LDY
        oddMap 5 _ = illOp
        oddMap 7 5 = pure LDY
        oddMap _ _ = illOp

opCode _ = Nothing

illOp :: Maybe Op
illOp = Nothing

illAddr :: Maybe AddrMode
illAddr = Nothing
