{-# LANGUAGE OverloadedStrings, GADTs, TypeOperators, FlexibleContexts, DataKinds, RankNTypes, ScopedTypeVariables #-}

module PPU.SDL
  (
    Draw
  , runDraw
  , pixel
  , Color
  , mkColor
  )where

import SDL
import Data.Word
import Linear (V4(..))

import Control.Monad (unless)
import Control.Monad.Freer
import Control.Arrow ((***))

import Foreign.C.Types(CInt)

type PixelScale = CInt

runRender :: PixelScale -> (Renderer -> IO a) -> IO a
runRender scale rma = do
  initializeAll
  let conf =
        defaultWindow { windowInitialSize = V2 (256*scale) (240*scale) }
  window <- createWindow "SampleApp" conf
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer (rma renderer)
  where appLoop :: Renderer -> IO a -> IO a
        appLoop r drawEff = do
          events <- pollEvents
          let qPressed ev =
                case eventPayload ev of
                  KeyboardEvent keyEv ->
                    keyboardEventKeyMotion keyEv == Pressed
                    && keysymKeycode (keyboardEventKeysym keyEv) == KeycodeQ
                  _ -> False
              exit = any qPressed events
          clear r
          out <- drawEff
          present r
          case any qPressed events of
            True -> return out
            False -> appLoop r drawEff

-- Prototyping
type Color = V4 Word8
type DrawEnv = (Renderer, PixelScale)

data Draw a where
  Pixel :: Integral b => (b,b) -> Color -> Draw ()

pixel :: (Member Draw r, Integral a) => (a,a) -> Color -> Eff r ()
pixel c = send . Pixel c

execDraw :: Draw a -> Word8 -> Renderer -> IO a
execDraw (Pixel coord c) s r = do
  prev <- get $ rendererDrawColor r
  rendererDrawColor r $= c
  out <- drawRect r shape >> fillRect r shape
  rendererDrawColor r $= prev
  where shape = let (x',y') = (fromIntegral *** fromIntegral) $ coord
                    s' = fromIntegral s
                in Just $ Rectangle (P (V2 x' y')) (V2 s' s')

runDraw :: forall a r. LastMember IO r
        => Eff (Draw ': r) a -> Eff r a
runDraw = interpret go
  where go :: Draw ~> Eff r
        go req = sendM $ runRender 2 $ execDraw req 2

mkColor :: (Word8,Word8,Word8) -> Color
mkColor (r,g,b) = V4 r g b 0xFF
