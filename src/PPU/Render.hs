{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

module PPU.Render
  (render
  ,Draw, DrawEnv
  ,liftDraw
  ,pixel
  ,inColor
  )where

import SDL
import Data.Word
import Linear (V4(..))
import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)

import Foreign.C.Types(CInt)

type PixelScale = CInt

render :: PixelScale -> IO ()
render scale = do
  initializeAll
  let conf =
        defaultWindow { windowInitialSize = V2 (256*scale) (240*scale) }
  window <- createWindow "SampleApp" conf
  render <- createRenderer window (-1) defaultRenderer
  appLoop (render,scale)

appLoop :: DrawEnv -> IO ()
appLoop denv@(r,s) = do
  events <- pollEvents
  let qPressed ev =
        case eventPayload ev of
          KeyboardEvent keyEv ->
            keyboardEventKeyMotion keyEv == Pressed
            && keysymKeycode (keyboardEventKeysym keyEv) == KeycodeQ
          _ -> False
      exit = any qPressed events
  clear r
  runDraw denv $ pixel 10 10
  present r
  unless exit (appLoop denv)

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

px :: Draw CInt
px = snd <$> ask

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

pixel :: CInt -> CInt -> Draw ()
pixel x y = do s <- px
               let rect = Just $ Rectangle (P (V2 x y)) (V2 s s)
                   white = V4 0xFF 0xFF 0xFF 0xFF
                   drawRec r = drawRect r rect >> fillRect r rect
               inColor white $ liftDraw drawRec
