{-# LANGUAGE GADTs, FlexibleContexts, DataKinds, TypeOperators, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module CPU.Sandbox where

import Util
import CPU.Decode
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.Internal (handleRelayS)
import Control.Monad.Freer.State
import Data.Word
import Lens.Micro.Platform
import qualified Data.Vector as V

---------
-- PPU --
---------
data PPUCtl a where
  WriteCTL :: Word8 -> PPUCtl ()
  --
  ReadSTAT :: PPUCtl Word8
  --
  WriteMask :: Word8 -> PPUCtl ()
  --
  WriteScroll :: Word8 -> PPUCtl ()
  --
  WritePPUAddr :: Word8 -> PPUCtl ()
  WritePPUData :: Word8 -> PPUCtl ()
  ReadPPUData :: PPUCtl Word8
  --
  WriteOAMAddr :: Word8 -> PPUCtl ()
  WriteOAMData :: Word8 -> PPUCtl ()
  ReadOAMData :: PPUCtl Word8

makeEffect ''PPUCtl

-- Handle PPU Stuff
type IOBus = Word8

runPPUCtl :: Eff (PPUCtl ': r) a -> Eff r (a,[String])
runPPUCtl = runState [] . reinterpret reint
  where reint :: PPUCtl ~> Eff (State [String] ': r)
        reint (WriteCTL v) = modify $ logg "WriteCTL"
        reint ReadSTAT = modify (logg "ReadStat") >> return 0
        reint (WritePPUAddr _) = modify $ logg "WriteAddr"
        reint (WritePPUData _) = modify $ logg "WriteData"
        reint ReadPPUData = modify (logg "ReadData") >> return 0
        reint _ = undefined
        --
        logg :: String -> [String] -> [String]
        logg = (++) . (:[])

---------
-- RAM --
---------
type Memory = V.Vector Word8

data RAM a where
  ReadRAM :: Word16 -> RAM Word8
  WriteRAM :: Word16 -> Word8 -> RAM ()

makeEffect ''RAM

-- We need to interpose RAM stuff with PPU Control
ppuControl :: forall a r. (Member RAM r, Member PPUCtl r)
           => Eff r a
           -> Eff r a
ppuControl req = interpose go req
  where go :: RAM ~> Eff r
        go (ReadRAM a) = if a `elem` [0x2002,0x2004,0x2007]
                         then mapRead a else readRAM a
        go (WriteRAM a v) = if a `elem` [0x2000,0x2001,0x2003,0x2005
                                        ,0x2004,0x2006,0x2007]
                            then mapWrite a v else writeRAM a v
        --
        mapRead :: Word16 -> Eff r Word8
        mapRead 0x2002 = readSTAT    -- Read PPU Status
        mapRead 0x2004 = readOAMData -- Read OAM Data
        mapRead 0x2007 = readPPUData -- Read PPU Data
        --
        mapWrite :: Word16 -> Word8 -> Eff r ()
        mapWrite 0x2000 = writeCTL
        mapWrite 0x2001 = writeMask    -- Write mask
        mapWrite 0x2003 = writeOAMAddr -- Write OAM Address
        mapWrite 0x2004 = writeOAMData -- Write OAM Data
        mapWrite 0x2005 = writeScroll  -- Write scroll
        mapWrite 0x2006 = writePPUAddr -- Write PPU Address
        mapWrite 0x2007 = writePPUData -- Write PPU Data

--
runRAM :: forall a r. Eff (RAM ': r) a -> Eff r a
runRAM req = handleRelayS initMem (const pure) go req
  where go :: Memory -> RAM v -> (Memory -> v -> Eff r b) -> Eff r b
        go m (ReadRAM a) f = f m (m V.! fromIntegral a)
        go m (WriteRAM a v) f = f (m V.// [(fromIntegral a,v)]) ()
        initMem = V.replicate 0xFFFF 0x0

reflectRAM :: forall a r. Member RAM r => Eff r a -> Eff r a
reflectRAM req = interpose go req
  where go :: RAM ~> Eff r
        go (ReadRAM a) = readRAM $ mirrorRAM a
        go (WriteRAM a v) = writeRAM (mirrorRAM a) v
        -- Mirroring Helpers
        mirrorRAM = mirrorRAM1 . mirrorRAM2
        mirrorRAM1 = (0x2000+) . mirror (0x2000,0x4000) 8
        mirrorRAM2 = mirror (0x0000,0x8000) 0x8000
        -- General mirroring
        mirror r m v = if v `within` r then v `mod` m else v
        -- Helper
        within :: Word16 -> (Word16,Word16) -> Bool
        within x (mn,mx) = x >= mn && x < mx

-- TEST AREA
runNES :: Eff '[PPUCtl,RAM] a -> (a,[String])
runNES = run . runRAM . runPPUCtl
         . ppuControl . reflectRAM -- Hijacking writes and reads
