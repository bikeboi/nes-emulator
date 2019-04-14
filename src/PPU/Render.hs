{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts, TypeOperators, DataKinds, ScopedTypeVariables #-}

module PPU.Render where

-- Rendering effect

import PPU.Data.Tile
import PPU.Internal

import Data.Word

import Control.Monad.Freer
import Control.Monad.Freer.State

data Render a where
  Display :: Tile k -> Render ()

display :: Member Render r => Tile a -> Eff r ()
display = send . Display

renderString :: forall r a. Eff (Render ': r) a -> Eff r (a,[String])
renderString = runState [] . reinterpret go
  where go :: Member (State [String]) r' => Render ~> Eff r'
        go (Display t@(Tile r)) = put ["beech"]

screen :: Tile Word16
screen = Tile $ map (\x -> map ((+ (8 * x * 256)) . (*8)) [0 .. 31]) [0 .. 29]
