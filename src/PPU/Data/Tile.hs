{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes, TypeOperators, MonoLocalBinds#-}

module PPU.Data.Tile where

-- Tile Representation

import Util
import Numeric (showIntAtBase, readHex)
import Data.Bits
import PPU.Internal

import Data.Char (digitToInt, intToDigit)
import Data.Word

import Lens.Micro.Platform ((^.))
import Control.Applicative (liftA2)
import Control.Arrow ((***), (&&&))
import Control.Monad.Freer

-- * Tile Construction
mkTile :: (Member VRAM r, Member ReadInternal r)
       => Word16 -> Eff r (Tile Word8)
mkTile x = do (nt,ntOff) <- nametable x
              color <- attrColor $ attrIx x ntOff
              (fmap . fmap) (const color) $ patternTable nt

-- * Nametable
nametable :: (Member VRAM r, Member ReadInternal r) => Word16 -> Eff r (Word8,Word16)
nametable x = do ms <- mirrors
                 let q = nametableQuad x ms
                 val <- readVRAM $ q + x `mod` 128
                 return (val,q)

nametableQuad :: Word16 -> Mirror -> Word16
nametableQuad x = let coords = fine (16 * 8) x
                  in flip (^.) (quad coords)
  where quad (0,0) = tl_
        quad (1,0) = tr_
        quad (0,1) = bl_
        quad (1,1) = br_

-- * Attribute Table
attrColor :: Member VRAM r => Attribute -> Eff r Word8
attrColor (addr,quad) = readVRAM addr >>= pure . attrColorDecode quad

attrColorDecode :: Quadrant -> Word8 -> Word8
attrColorDecode q = (.&.) 3 . flip (.>>.) (sec q)
  where sec TL = 0
        sec TR = 2
        sec BR = 4
        sec BL = 6

-- | Calculates position in attribute table
attrIx :: Word16 -> Word16 -> Attribute
attrIx n offs = (,) <$> pos <*> quad $ n
  where pos n = let (x,y) = fine 32 n
                in y * 8 + 0xc0 + x
        quad n = case even *** even $ fine 16 n of
                   (True,True)   -> TL
                   (False,True)  -> TR
                   (True,False)  -> BL
                   (False,False) -> BR

data Quadrant = TL | TR | BL | BR deriving (Eq, Show)
type Attribute = (Word16, Quadrant)

-- * Pattern Table
patternTable :: (Member VRAM r, Member ReadInternal r)
             => Word8 -> Eff r (Tile Int)
patternTable x = pattTable BG >>= mkCHR x

mkCHR :: Member VRAM r => Word8 -> Word16 -> Eff r (Tile Int)
mkCHR addr offs = do let addr' = (+ offs) $ to16 $ addr .<<. 4
                     hi <- mapM readVRAM [addr' .. addr'+7]
                     lo <- mapM readVRAM [addr'+8 .. addr'+16]
                     return $ Tile $ zipWith mkSliver hi lo

mkSliver :: Word8 -> Word8 -> Sliver Int
mkSliver a b = map (\i -> combine (a `cbit` i) (b `cbit` i)) [7,6..0]
  where combine False False = 0
        combine False True  = 1
        combine True  False = 2
        combine _     _     = 3

type Sliver a = [a]

newtype Tile a =
  Tile [Sliver a]
  deriving Eq

instance Functor Tile where
  fmap f (Tile sliver) = Tile $ (fmap . fmap) f sliver

ppTile :: Show a => Tile a -> IO ()
ppTile (Tile aas) = mapM_ print aas

type CHR = Tile Word8

-- | General Helpers
type Interval = Word16

fine :: Interval -> Word16 -> (Word16,Word16)
fine it = (,)
          <$> (`div` it) . (.&. 0x00ff) -- X
          <*> (`div` it) . (.>>. 8)     -- Y

interval :: Interval -> Word16 -> Word16
interval = flip div
