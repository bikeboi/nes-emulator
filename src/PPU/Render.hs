{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts, TypeOperators, DataKinds, ScopedTypeVariables #-}

module PPU.Render
  ( renderTile
  , runDraw
  , Draw) where

-- Rendering effect
import Data.Word
import Numeric

import PPU.Data.Tile
import PPU.Internal
import PPU.SDL

import qualified Data.Map as M

import Control.Arrow ((&&&), (***), (>>>))
import Control.Monad.Freer
import Control.Monad.Freer.State

renderTile :: ( Member VRAM r
              , Member ReadInternal r
              , Member Draw r)
           => (Word16,Word16) -> Eff r ()
renderTile xy = do colorIxs <- buildTile $ toTileIx xy
                   case mapTileM (\i -> mkColor <$> M.lookup i pallette) colorIxs of
                     Nothing -> error "UMMMM"
                     Just t -> mapTileM_ (pixel xy) t

pallette :: M.Map Word8 (Word8,Word8,Word8)
pallette = M.fromList $ zip [0 ..]
  [(84,84,84),(0,30,116),(8,16,144),(48,0,136),(68,0,100),(92,0,48)
  ,(84,4,0),(60,24,0),(32,42,0),(8,58,0),(0,64,0),(0,60,0),(0,50,60)
  ,(0,0,152),(150,152,8),(76,196,48),(50,236,92),(30,228,136),(20,176,160)
  ,(20,100,152),(34,32,120),(60,0,84),(90,0,40),(114,0,8),(124,0,0)
  ,(118,40,0),(102,120,0),(0,0,236),(238,236,76),(154,236,120),(124,236,176)
  ,(98,236,228),(84,236,236),(88,180,236),(106,100,212),(136,32,160)
  ,(170,0,116),(196,0,76),(208,32,56),(204,108,56),(180,204,60),(60,60,236)
  ,(238,236,168),(204,236,188),(188,236,212),(178,236,236),(174,236,236)
  ,(174,212,236),(180,176,228),(196,144,204),(210,120,180),(222,120,168)
  ,(226,144,152),(226,180,160),(214,228,160)]

tile :: Tile (Word16,Word16)
tile = mkTile 8 8 (,)

-- | Tile as sequence of bytes
tileBytes :: Tile Word16
tileBytes = fmap toBytes tile

-- | Screen Coordinates
screenXY :: Tile (Word16,Word16)
screenXY = mkTile 30 32 (,)

-- | Screen as sequence of bytes
screenBytes :: Tile Word16
screenBytes = fmap toTileIx screenXY

toBytes :: (Word16,Word16) -> Word16
toBytes (x,y) = y * 8 + x

toTileIx :: (Word16,Word16) -> Word16
toTileIx = ((*8) *** (*256)) >>> toBytes
--
{-
-}
