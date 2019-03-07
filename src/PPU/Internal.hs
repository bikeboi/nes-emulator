{-# LANGUAGE BinaryLiterals, TemplateHaskell #-}

module PPU.Internal where

import Data.Word

import Lens.Micro.Platform

-- PPU Internal Mechanisms

-- | PPU Registers, the inner state of the processor
data PPUnit =
  PPUnit { _ppuCtrl :: Word8  -- ^ $2000
         , _ppuMask :: Word8  -- ^ $2001
         , _ppuStat :: Word8  -- ^ $2002
         , _oamAddr :: Word8  -- ^ $2003
         , _oamData :: Word8  -- ^ $2004
         , _ppuScrl :: Word16 -- ^ $2005
         , _ppuAddr :: Word16 -- ^ $2006
         , _ppuData :: Word8  -- ^ $2007
         , _oamDmah :: Word8  -- ^ $4014
         } deriving (Eq, Show)

-- | PPU state after power-on or reset
initPPUnit :: PPUnit
initPPUnit =
  PPUnit { _ppuCtrl = 0x00
         , _ppuMask = 0x00
         , _ppuStat = 0b1010000
         , _oamAddr = 0x00
         , _oamData = 0x00
         , _ppuScrl = 0x0000
         , _ppuAddr = 0x0000
         , _ppuData = 0x00
         , _oamDmah = 0x00 }

makeLenses ''PPUnit
