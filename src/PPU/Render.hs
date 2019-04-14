{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module PPU.Render
  ( render
  , Draw, DrawEnv
  , liftDraw
  , pxScale
  , square
  , pixel
  , inColor
  , mkColor
  )where

import SDL
import Data.Word
import Linear (V4(..))
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)

import Foreign.C.Types(CInt)

type PixelScale = CInt

render :: PixelScale -> Draw () -> IO ()
render scale dact = do
  initializeAll
  let conf =
        defaultWindow { windowInitialSize = V2 (256*scale) (240*scale) }
  window <- createWindow "SampleApp" conf
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer dact
  where appLoop :: Renderer -> Draw () -> IO ()
        appLoop r dact = do
          events <- pollEvents
          let qPressed ev =
                case eventPayload ev of
                  KeyboardEvent keyEv ->
                    keyboardEventKeyMotion keyEv == Pressed
                    && keysymKeycode (keyboardEventKeysym keyEv) == KeycodeQ
                  _ -> False
              exit = any qPressed events
          clear r
          runDraw (r,scale) dact
          present r
          unless exit (appLoop r dact)

-- Prototyping
newtype Draw a =
  Draw { unDraw :: ReaderT DrawEnv IO a }
  deriving (Functor,Applicative,Monad
           ,MonadReader DrawEnv
           ,MonadIO)

type DrawEnv = (Renderer, PixelScale)
type Color = V4 Word8

runDraw :: DrawEnv -> Draw a -> IO a
runDraw r d = runReaderT (unDraw d) r

rend :: Draw Renderer
rend = fst <$> ask

pxScale :: Draw CInt
pxScale = snd <$> ask

liftDraw :: (Renderer -> IO a) -> Draw a
liftDraw f = do r <- rend
                liftIO $ f r

-- COMBINATORS
inColor :: Color -> Draw a -> Draw a
inColor c drw = do r <- rend
                   prev <- get $ rendererDrawColor r
                   rendererDrawColor r $= c
                   a <- drw
                   rendererDrawColor r $= prev
                   return a

mkColor :: (Word8,Word8,Word8) -> Color
mkColor (r,g,b) = V4 r g b 0xFF

square :: Integral a => a -> a -> a -> Draw ()
square s x y = do sc <- (* fromIntegral s) <$> pxScale
                  let (x',y') = (fromIntegral x, fromIntegral y)
                  let shape = Just $ Rectangle (P (V2 x' y')) (V2 sc sc)
                  liftDraw $ \rend -> drawRect rend shape >> fillRect rend shape

pixel :: Integral a => a -> a -> Draw ()
pixel = square 1
