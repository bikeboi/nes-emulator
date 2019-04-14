{-# LANGUAGE TemplateHaskell, GADTs, ScopedTypeVariables, FlexibleContexts, TypeOperators, DataKinds, QuasiQuotes, BinaryLiterals #-}

module PPU.Sandbox ( sandbox,PPUReg(..)
                   , readSTAT, readOAMData,readPPUData
                   , writeCTL, writeMask
                   , writeOAMAddr, writeOAMData
                   , writeScroll
                   , writePPUAddr, writePPUData) where

import Util
import Numeric (readHex)
import Data.Bits ((.&.),(.|.))
import PPU.Render

import Data.Char (digitToInt)
import qualified Data.Vector as V
import Data.Word
import qualified Data.ByteString as B
import Text.RawString.QQ
import Control.Applicative (liftA3)
import Control.Monad (forM_)
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
  WritePPUAddr :: Word8 -> PPUReg ()
  WritePPUData :: Word8 -> PPUReg ()
  ReadPPUData :: PPUReg Word8
  --
  WriteOAMAddr :: Word8 -> PPUReg ()
  WriteOAMData :: Word8 -> PPUReg ()
  ReadOAMData :: PPUReg Word8

makeEffect ''PPUReg

runPPUReg :: forall a r. (Member (State PPUInt) r, Member VRAM r)
          =>  Eff (PPUReg ': r) a -> Eff r a
runPPUReg = interpret go
  where go :: PPUReg ~> Eff r
        go (WriteCTL c) = modify $ (ppuCTL .~ PPUCTL c) . latch c
        go ReadSTAT = unlatch >> get >>= pure . ((^.) ppuSTAT . unwrapSTAT)
        go _ = undefined
        --
        latch n = ppuAddrLatch %~ writeLatch n
        unlatch :: Eff r ()
        unlatch = modify $ ppuAddrLatch .~ emptyLatch

-- Get nametable entry

-- COLOR PALETTE
type Color = (Word8,Word8,Word8)
type Pallette = (Color,Color,Color)

parsePallette :: [Word8] -> [Color]
parsePallette = zipWith3 (,,) <$> skip3 <*> skip3 . tail <*> skip3 . tail . tail
  where skip3 = map snd . filter ((==0) . (`mod` 3) . fst) . zip [0..]

-- TOP LEVEL
tileColorIndex :: forall r. (Member (State PPUInt) r, Member VRAM r)
          => Eff r Tile
tileColorIndex =
  get
  >>= calcColorIndex <$> (^. ppuLaser) <*> (^. ppuNTMirror)
  >>= pure . fmap ((V.!) pallettes . fromIntegral)
  where pallettes = V.fromList $ parsePallette $ map (fst . head . readHex) $ words "5252 0052 5e1b 0d0e 2677 7403 003c 4956 0000010 2901 0748 3801 0013 2120 0900 002e 3500 0000 0733 2900 0032 0000 0000 0000 0000 a0a0 17a0 b14b 3436 5cd6 d223 1b7f 92a5 611d 2990 781e 003e 5553 2e00 0069 7312 0600 2a70 6107 006f 0000 0000 0000 0000 fffe 60ff ff9f 8486 b3ff ff6f 65da efff b868 77ec d269 2e90 aba9 7d13 16c1 cd5a 4837 78c9 b84a 3cc8 3c3c 0000 0000 0000 fffe baff ffd6 cacb dfff ffc1 bcef f8ff e1be c5f7 ecbe a2cf dbda c793 94e5 eab7 afa6 c5e8 e1b0 a9e8 a9a9 0000 0000 0000"

type ATEntry = Word8
type NTEntry = Word16

calcColorIndex :: forall r. (Member VRAM r, Member (State PPUInt) r)
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
  Tile (V.Vector (V.Vector a))
  deriving Show

instance Functor TileRep where
  fmap f (Tile xs) = Tile $ (fmap . fmap) f xs


type CHR = TileRep Word8
type Tile = TileRep Color

calcIndex :: forall r. Member (State PPUInt) r
          => CHR -> ATEntry -> Eff r CHR -- remember to get bg or spr picker
calcIndex patt at = pure $ fmap (\p -> p .|. (at .<<. 2)) patt

getPT :: forall r. Member VRAM r => Word8 -> Eff r CHR
getPT x = Tile <$> V.mapM getPT' (V.generate 7 ((+x) . fromIntegral))
  where getPT' :: Word8 -> Eff r (V.Vector Word8)
        getPT' nval = do
          let paddr = to16 nval .<<. 1
          ab <- (,) <$> readVRAM paddr <*> readVRAM (paddr + 8)
          pure . uncurry mkCHR $ ab

mkCHR :: Word8 -> Word8 -> V.Vector Word8
mkCHR a b = let f = fmap (to8 . digitToInt) . V.fromList . show2
                (as,bs) = (f a,f b)
            in V.zipWith (\h l -> (h .<<. 1) .|. l) as bs

sandbox :: IO ()
sandbox = print $ test $ tileColorIndex -- >>= pure . (V.!) pallettes . fromIntegral

-- RENDERING
drawTile :: Tile -> (Word8,Word8) -> Draw ()
drawTile (Tile arr) (xoff,yoff)= V.zipWithM_ drawRows xoffs arr
  where drawRows x r = V.zipWithM_ (drawPix x) yoffs r
        drawPix x y c = inColor (mkColor c) $ pixel x y
        xoffs = V.generate 8 ((+ xoff) . fromIntegral)
        yoffs = V.generate 8 ((+ yoff) . fromIntegral)

test = run . runVRAM_ . runState initPPUInt . runPPUReg
