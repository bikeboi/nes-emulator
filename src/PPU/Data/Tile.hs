{-# LANGUAGE DataKinds, FlexibleContexts, RankNTypes, TypeOperators, MonoLocalBinds#-}

module PPU.Data.Tile where

-- Tile Representation

import Util
import Numeric (showIntAtBase, readHex)
import Data.Bits
import PPU.Internal (VRAM,readVRAM,writeVRAM)

import Data.Char (digitToInt, intToDigit)
import Data.Word

import Control.Applicative (liftA2)
import Control.Arrow ((***))
import Control.Monad.Freer

-- * Nametable


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
  where pos n = let (c,r) = fine 32 n
                in r * 8 + 0xc0 + c
        quad n = case even *** even $ fine 16 n of
                   (True,True)   -> TL
                   (False,True)  -> TR
                   (True,False)  -> BL
                   (False,False) -> BR

data Quadrant = TL | TR | BL | BR deriving (Eq, Show)
type Attribute = (Word16, Quadrant)

-- * Pattern Table

type Sliver a = [a]

newtype Tile a =
  Tile [Sliver a]
  deriving Eq

instance Functor Tile where
  fmap f (Tile sliver) = Tile $ (fmap . fmap) f sliver

ppTile :: Show a => Tile a -> IO ()
ppTile (Tile aas) = mapM_ print aas

type CHR = Tile Word8

mkCHR :: Member VRAM r => Word8 -> Eff r (Tile Int)
mkCHR addr = do let addr' = to16 $ addr .<<. 4
                hi <- mapM readVRAM [addr' .. addr'+7]
                lo <- mapM readVRAM [addr'+8 .. addr'+16]
                return $ Tile $ zipWith mkSliver hi lo

mkSliver :: Word8 -> Word8 -> Sliver Int
mkSliver a b = map (\i -> combine (a `cbit` i) (b `cbit` i)) [7,6..0]
  where combine False False = 0
        combine False True  = 1
        combine True  False = 2
        combine _     _     = 3

-- | General Helpers
type Interval = Word16

fine :: Interval -> Word16 -> (Word16,Word16)
fine it x = let col = toCol it $ x `mod` 256
                row = toRow it col x
            in (col,row)
  where toCol i a = a .>>. (truncate . logBase (2 :: Float) . fromIntegral) i
        toRow i c a = (a .&. 0xff00) `div` 255 `div` i
