{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, TypeOperators, DataKinds, QuasiQuotes, BinaryLiterals #-}

module PPU.Sandbox ( sandbox,PPUCtl(..)
                   , readSTAT, readOAMData,readPPUData
                   , writeCTL, writeMask
                   , writeOAMAddr, writeOAMData
                   , writeScroll
                   , writePPUAddr, writePPUData) where

import Util
import Numeric (readHex)
import Data.Bits ((.&.),(.|.))
import PPU.Render

import qualified Data.Vector as V
import Data.Word
import qualified Data.ByteString as B
import Text.RawString.QQ
import Control.Applicative (liftA3)
import Control.Monad ((>=>))
import Control.Arrow ((&&&),first)

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Freer.State

import Lens.Micro.Platform

-- PPU has some internal state I'm not fully aware of
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

data LatchStat = W0 | W1 | W2 deriving (Eq, Ord, Show)

instance Enum LatchStat where
  toEnum x = case x `mod` 2 of
               0 -> W2
               1 -> W1
  fromEnum W0 = 0
  fromEnum W1 = 1
  fromEnum W2 = 2

-- Address Latch
type Latch = (Word8,LatchStat)

initLatch :: Latch
initLatch = (0x00,W0)

writeLatch :: Word8 -> Latch -> Latch
writeLatch v latch = latch & (_1 .~ v) . (_2 %~ succ)

-----------------
-- PPU CONTROL --
-----------------
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

runPPUCtl :: forall a r. Eff (PPUCtl ': r) a -> Eff r a
runPPUCtl = interpret go
  where go :: PPUCtl ~> Eff r
        go = undefined

-- PPU Internal State
newtype NTMirror =
  NTMirror (Word16 -- TOP LEFT
           ,Word16 -- TOP RIGHT
           ,Word16 -- BOT LEFT
           ,Word16 -- BOT RIGHT
           ) deriving (Eq, Show)

ntVert :: NTMirror
ntVert = NTMirror (0x2000,0x2400,0x2000,0x2400)

ntHorz :: NTMirror
ntHorz = NTMirror (0x2000,0x2000,0x2800,0x2800)

data PPUIntern =
  PPUIntern { _ppuLaser :: (Word8,Word8)
            , _ppuAddrLatch :: Latch
            , _ppuNTMirror :: NTMirror } deriving Show

initPPUIntern :: PPUIntern
initPPUIntern = PPUIntern (0,0) initLatch ntVert

makeLenses ''PPUIntern

-- Get nametable entry

-- COLOR PALETTE
parsePallette :: [Word8] -> [Color]
parsePallette = zipWith3 (,,) <$> skip3 <*> skip3 . tail <*> skip3 . tail . tail
  where skip3 = map snd . filter ((==0) . (`mod` 3) . fst) . zip [0..]

-- TOP LEVEL
tileColorIndex :: forall r. (Member (State PPUIntern) r, Member VRAM r)
          => Eff r Tile
tileColorIndex =
  get
  >>= calcColorIndex <$> (^. ppuLaser) <*> (^. ppuNTMirror)
  >>= pure . fmap ((V.!) pallettes . fromIntegral)
  where pallettes = V.fromList $ parsePallette $ map (fst . head . readHex) $ words "5252 0052 5e1b 0d0e 2677 7403 003c 4956 0000010 2901 0748 3801 0013 2120 0900 002e 3500 0000 0733 2900 0032 0000 0000 0000 0000 a0a0 17a0 b14b 3436 5cd6 d223 1b7f 92a5 611d 2990 781e 003e 5553 2e00 0069 7312 0600 2a70 6107 006f 0000 0000 0000 0000 fffe 60ff ff9f 8486 b3ff ff6f 65da efff b868 77ec d269 2e90 aba9 7d13 16c1 cd5a 4837 78c9 b84a 3cc8 3c3c 0000 0000 0000 fffe baff ffd6 cacb dfff ffc1 bcef f8ff e1be c5f7 ecbe a2cf dbda c793 94e5 eab7 afa6 c5e8 e1b0 a9e8 a9a9 0000 0000 0000"

type ATEntry = Word8
type NTEntry = Word16

calcColorIndex :: forall r. (Member VRAM r, Member (State PPUIntern) r)
              => (Word8,Word8) -> NTMirror -> Eff r CHR
calcColorIndex xy ntmirr = do
  let quad = ntQuadrant xy ntmirr
  ntval <- readVRAM $ ntAddr xy quad
  atval <- getAT xy quad
  ptval <- getPT ntval
  calcIndex ptval atval
  where ntQuadrant :: (Word8,Word8) -> NTMirror -> Word16
        ntQuadrant (x,y) (NTMirror (tl,tr,bl,br)) =
          case (x > 16, y > 15) of
            (False,False) -> tl
            (True,False)  -> tr
            (False,True)  -> bl
            (True,True)   -> br

ntAddr :: (Word8,Word8) -> Word16 -> NTEntry
ntAddr (x,y) offset = offset + (fromIntegral x + fromIntegral y * 16)

getAT :: Member VRAM r => (Word8,Word8) -> Word16 -> Eff r ATEntry
getAT (x,y) offset = let
  (x',y') = (fromIntegral x,fromIntegral y)
  addr = (offset + 0x3C0) + x' `div` 2 + (y' `div` 2) * 8
  (mask,shift) = case (even x, even y) of
                    (True,True)   -> (0b00000011,0)
                    (False,True)  -> (0b00001100,2)
                    (True,False)  -> (0b00110000,4)
                    (False,False) -> (0b11000000,6)
  in (.>>. shift) . (.&. mask) <$> readVRAM addr

newtype TileRep a =
  Tile [[a]]
  deriving Show

instance Functor TileRep where
  fmap f (Tile xs) = Tile $ (fmap . fmap) f xs

type CHR = TileRep Word8
type Tile = TileRep Color

calcIndex :: forall r. Member (State PPUIntern) r
          => CHR -> ATEntry -> Eff r CHR -- remember to get bg or spr picker
calcIndex patt at = pure $ fmap (\p -> p .|. (at .<<. 2)) patt

getPT :: forall r. Member VRAM r => Word8 -> Eff r CHR
getPT x = Tile <$> mapM getPT' [x .. x+7]
  where getPT' :: Word8 -> Eff r [Word8]
        getPT' nval = do
          let paddr = to16 nval .<<. 1
          (a,b) <- (,) <$> readVRAM paddr <*> readVRAM (paddr + 8)
          let as = map (read . (:[])) $ show2 a
              bs = map (read . (:[])) $ show2 b
          pure $ zipWith (\lf rh -> (lf .<<. 1) .|. rh) as bs

sandbox :: IO ()
sandbox = print $ test $ tileColorIndex -- >>= pure . (V.!) pallettes . fromIntegral

type Color = (Word8,Word8,Word8)
type Pallette = (Color,Color,Color)

test = run . runVRAM_ . runState initPPUIntern
