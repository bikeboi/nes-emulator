{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts #-}

module PPU where

import PPU.Render
import PPU.Internal

import Control.Monad.Freer
import Control.Monad.Freer.State

-- This probably takes some parameters from the CPU i've forgotten about
runPPU :: LastMember IO r
       => Eff (PPUReg : ReadInternal : State Internals : VRAM : Draw : r) a
       -> Eff r (a, Internals)
runPPU = runDraw . runInternal
