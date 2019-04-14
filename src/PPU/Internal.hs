{-# LANGUAGE BinaryLiterals, TemplateHaskell, RankNTypes, GADTs, FlexibleContexts, TypeOperators, DataKinds, ScopedTypeVariables #-}

module PPU.Internal
  ( -- Video RAM
    VRAM
  , readVRAM
  , writeVRAM
  , runVRAM
    -- ReadInternal State
  , ReadInternal
  , baseNT, addrInc, pattTable, sprLong, genNMI
  , greyScale, showSliver, showTile, emph
  , Shade (..), TileType(..)
    -- Helpers
  , tr_, tl_, br_, bl_
  ) where

import Util
import Data.Bits ((.&.))
import Data.Word
import qualified Data.Vector as V

import Control.Monad.Freer
import Control.Monad.Freer.State
import Control.Monad.Freer.TH
import Lens.Micro.Platform hiding (view)

---------
-- * VRAM
---------
type VideoRAM = V.Vector Word8

data VRAM a where
  ReadVRAM :: Word16 -> VRAM Word8
  WriteVRAM :: Word16 -> Word8 -> VRAM ()

makeEffect ''VRAM

runVRAM :: forall a r. Eff (VRAM ': r) a -> Eff r (a,V.Vector Word8)
runVRAM = runState initVRAM . reinterpret go
  where go :: VRAM ~> Eff (State VideoRAM ': r)
        go (ReadVRAM v) = (<$> get). flip (V.!) . fromIntegral . handleMirrors $ v
        go (WriteVRAM a v) = modify (V.// [(fromIntegral $ handleMirrors a,v)])
        --
        handleMirrors :: Word16 -> Word16
        handleMirrors x | x `within` (0x3000,0x3F00) = x - 0x1000
                        | x `within` (0x3F20,0x4000) = x `mod` 32 + 0x3F00
                        | otherwise = x
        --
        within x (mn,mx) = x >= mn && x < mx
        initVRAM = V.replicate 0x3FFF 0

-- HARDCODED BOIIIISS
runVRAM_ :: forall a r. Eff (VRAM ': r) a -> Eff r a
runVRAM_ = fmap fst . runVRAM

-----------------------
-- * PPU ReadInternal State
-----------------------
data Shade = Red | Green | Blue deriving (Eq,Show,Enum)
data TileType = SPR | BG deriving (Eq, Show)

-- | PPU Control Register
newtype Ctl = Ctl Word8 deriving (Eq, Show)
ctl_BaseNT :: SimpleGetter Ctl Word16
ctl_BaseNT = to $ \(Ctl n) -> case n .&. 0b11 of
                                   0 -> 0x2000
                                   1 -> 0x2400
                                   2 -> 0x2800
                                   3 -> 0x2C00

ctl_AddrInc :: SimpleGetter Ctl Word16
ctl_AddrInc = to $ \(Ctl n) -> if cbit n 2 then 32 else 1

ctl_SPattAddr :: SimpleGetter Ctl Word16
ctl_SPattAddr = to $ \(Ctl n) -> if cbit n 3 then 0x1000 else 0

ctl_BPattAddr :: SimpleGetter Ctl Word16
ctl_BPattAddr = to $ \(Ctl n) -> if cbit n 4 then 0x1000 else 0

ctl_SprLong :: SimpleGetter Ctl Bool
ctl_SprLong = to $ \(Ctl n) -> cbit n 5

ctl_NMI :: SimpleGetter Ctl Bool
ctl_NMI = to $ \(Ctl n) -> if cbit n 7 then True else False

-- | PPU STATUS
newtype Status =
  Status Word8
  deriving (Eq, Show)

stat :: Lens' Status Word8
stat = lens (\(Status x) -> x) (\(Status x) x' -> Status x')

-- | PPU MASK
newtype Mask =
  Mask Word8
  deriving (Eq, Show)

cmask :: Int -> Mask -> Bool
cmask n (Mask x) = cbit x n

msk_Grey :: SimpleGetter Mask Bool
msk_Grey = to $ cmask 0

msk_Sliver :: TileType -> SimpleGetter Mask Bool
msk_Sliver SPR = to $ cmask 2
msk_Sliver BG = to $ cmask 1

msk_Show :: TileType -> SimpleGetter Mask Bool
msk_Show SPR = to $ cmask 4
msk_Show BG = to $ cmask 3

msk_Emph :: Shade -> SimpleGetter Mask Bool
msk_Emph s = to $ cmask $ fromEnum s + 5

-- Auxilliary types
data LatchStat = W0 | W1 | W2 deriving (Eq, Ord, Show, Enum)

-- | Address Latch
type Latch = (Word16,LatchStat)

-- | Write to latch
writeLatch :: Word8 -> Latch -> Latch
writeLatch v latch = case latch ^. _2 of
                       W1 -> latch & (_1 +~ to16 v) . (_2 .~ W2)
                       _ -> (to16 v .<<. 8, W1)

emptyLatch :: Latch
emptyLatch = (0,W0)

-- | Nametable Mirroring
type Mirror = (Word16,Word16,Word16,Word16)

tl_, tr_, bl_, br_ :: SimpleGetter Mirror Word16
tl_ = _1
tr_ = _2
br_ = _3
bl_ = _4

data Internals =
  Internals { _ppuLaser :: Word16
            , _ppuLatch :: Latch
            , _ppuMirror :: Mirror
            , _ppuCTL :: Ctl
            , _ppuSTAT :: Status
            , _ppuMask :: Mask
            , _ppuScroll :: Word8 } deriving Show

-- This should take some ROM data
intern :: Internals
intern = Internals 0 emptyLatch (0x2000,0x2400,0x2800,0x2C00)
         (Ctl 0) (Status 0) (Mask 0) 0

makeLenses ''Internals

------------------
-- * PPU ACCESS --
------------------
-- | Read from Internals
data ReadInternal a where
  BaseNT :: ReadInternal Word16
  AddrInc :: ReadInternal Word16
  PattTable :: TileType -> ReadInternal Word16
  SprLong :: ReadInternal Bool
  GenNMI :: ReadInternal Bool
  --
  GreyScale :: ReadInternal Bool
  ShowSliver :: TileType -> ReadInternal Bool
  ShowTile :: TileType -> ReadInternal Bool
  Emph :: Shade -> ReadInternal Bool

makeEffect ''ReadInternal

runReadInternal :: forall a r. Member (State Internals) r
            => Eff (ReadInternal ': r) a -> Eff r a
runReadInternal = interpret go
  where go :: forall k. ReadInternal k -> Eff r k
        go req = get <&> see (f req)
        --
        f :: ReadInternal k -> SimpleGetter Internals k
        f BaseNT = ppuCTL . ctl_BaseNT
        f AddrInc = ppuCTL . ctl_AddrInc
        f (PattTable BG) = ppuCTL. ctl_BPattAddr
        f (PattTable SPR) = ppuCTL . ctl_SPattAddr
        f SprLong = ppuCTL . ctl_SprLong
        f GenNMI = ppuCTL . ctl_NMI
        -- MASK STUFF
        f GreyScale = ppuMask . msk_Grey
        f (ShowSliver s) = ppuMask . msk_Sliver s
        f (ShowTile t) = ppuMask . msk_Show t
        f (Emph s) = ppuMask . msk_Emph s
        --
        see = flip (^.)

-----------------
-- PPU CONTROL --
-----------------
data PPUReg a where
  WriteCTL :: Word8 -> PPUReg ()
  --
  ReadSTAT :: PPUReg Word8
  --
  WriteMask :: Word8 -> PPUReg ()
  --
  WriteScroll :: Word8 -> PPUReg ()
  --
  WriteAddr :: Word8 -> PPUReg ()
  WriteData :: Word8 -> PPUReg ()
  ReadData :: PPUReg Word8
  --
  WriteOAMAddr :: Word8 -> PPUReg ()
  WriteOAMData :: Word8 -> PPUReg ()
  ReadOAMData :: PPUReg Word8

makeEffect ''PPUReg

runPPUReg :: forall a r.
             ( Member (State Internals) r
             , Member ReadInternal r
             , Member VRAM r)
          =>  Eff (PPUReg ': r) a -> Eff r a
runPPUReg = interpret go
  where go :: PPUReg ~> Eff r
        go (WriteCTL x) = modify $ ppuCTL .~ (Ctl x)
        go ReadSTAT = get <&> (^. ppuSTAT . stat)
        go (WriteMask m) = modify $ ppuMask .~ (Mask m)
        go (WriteScroll x) = modify $ ppuScroll .~ x
        go (WriteAddr x) = modify $ ppuLatch %~ writeLatch x
        go (WriteData x) = do (a,ws) <- get <&> (^. ppuLatch)
                              case ws of
                                W2 -> writeVRAM a x
                                _ -> modify ((ppuLatch . _2) %~ succ)
        go ReadData = do inc <- addrInc
                         out <- get >>= (readVRAM . (^. ppuLatch . _1))
                         modify ((ppuLatch . _1) +~ inc)
                         return out

        -- OAM STUFF

-- INTERNAL
runInternals :: Eff (State Internals ': r) a -> Eff r (a,Internals)
runInternals = runState intern

-- BIG PIC
runInternal :: Eff (PPUReg : ReadInternal : State Internals : VRAM : r) a -> Eff r (a, Internals)
runInternal = runVRAM_ . runInternals . runReadInternal . runPPUReg
