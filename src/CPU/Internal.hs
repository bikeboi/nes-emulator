{-# LANGUAGE GeneralizedNewtypeDeriving
             , TemplateHaskell
             , RecordWildCards
             , BinaryLiterals
             , GADTs
             , TypeSynonymInstances
             , MultiParamTypeClasses
             , FlexibleInstances
             , RankNTypes #-}

module CPU.Internal (
  Processor, Interrupt(..)
  , CPU, runCPU, proccessor, cpuErr
  , readRAM, writeRAM, loadChunkRAM, initRAM
  , getA, getX, getY, getSP, getPS, getPC, getIR
  , mutA, mutX, mutY, mutSP, mutPS, mutPC
  , setA, setX, setY, setSP, setPS, setPC, setIR
  , pushStack, popStack
  , eat8, eat16
  , setN, setV, setB, setI, setZ, setC
  , clrN, clrV, clrB, clrI, clrZ, clrC
  , sN, sV, sB, sI, sZ, sC) where

import Util
import Memory
import Data.Word
import Data.Int
import Data.Bits hiding (bit)
import Data.Monoid (mconcat)
import qualified Data.ByteString as B

import Lens.Micro.Platform
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.IO.Class (liftIO)

-- Data Types
data Processor =
  Processor { _ps :: Word8
            , _a  :: Word8
            , _x  :: Word8
            , _y  :: Word8
            , _sp :: Word8
            , _pc :: Word16
            , _ir :: Maybe Interrupt } deriving Eq

data Interrupt = NMI | RST | IRQ deriving (Eq, Show)

makeLenses ''Processor

instance Show Processor where
  show Processor{..} = mconcat . map (\(a,b) -> a ++ b ++ " ") $
                       [("A:", show16 _a)
                       ,("X:", show16 _x)
                       ,("Y:", show16 _y)
                       ,("PC:", showBig16 _pc)
                       ,("SP:", show16 _sp)
                       ,("STAT: ", show2 _ps)]

initProcessor :: Processor
initProcessor =
  Processor { _ps = 0b00000100
            , _a   = 0x00
            , _x   = 0x00
            , _y   = 0x00
            , _sp  = 0xFD
            , _pc   = 0x6000
            , _ir = Nothing }

-- The CPU s Monad

type CPU s =
  ExceptT String (ReaderT (Memory s) (StateT Processor (ST s)))

runCPU :: (forall s. CPU s a) -> (Either String a,Processor)
runCPU op = runST $ do
  mem <- initRAM
  runStateT (runReaderT (runExceptT op) mem) initProcessor

cpuErr :: String -> CPU s a
cpuErr = throwError

-- Combinators Galore
proccessor :: CPU s Processor
proccessor = get

-- Interrupts
setIR :: Interrupt -> CPU s ()
setIR NMI = setIR NMI
setIR ir = do i <- sI
              if i then return () else setIR ir

-- Memory
liftST :: ST s a -> CPU s a
liftST = lift . lift . lift

initRAM :: ST s (Memory s)
initRAM = newMem 0xFFFF

readRAM :: Word16 -> CPU s Word8
readRAM a = do mem <- ask
               liftST $ readMem mem (mirrorRAM a)

writeRAM :: Word16 -> Word8 -> CPU s ()
writeRAM a v = do mem <- ask
                  liftST $ writeMem mem (mirrorRAM a) v

loadChunkRAM :: B.ByteString -> Word16 -> CPU s ()
loadChunkRAM b start = do
  mem <- ask
  liftST $ loadChunk b mirrorRAM start mem

mirrorRAM = mirrorRAM1 . mirrorRAM2
  where
    mirrorRAM1 :: Word16 -> Word16
    mirrorRAM1 = mirror (0x0800,0x1800) 0x07FF 0x0000

    mirrorRAM2 :: Word16 -> Word16
    mirrorRAM2 = mirror (0x2008,0x4000) 0x0008 0x2000

-- General CPU s access
getP :: SimpleGetter Processor a -> CPU s a
getP s = view s <$> proccessor

getPS, getX, getY, getA, getSP :: CPU s Word8
getPS  = getP ps
getA   = getP a
getX   = getP x
getY   = getP y
getSP  = getP sp

getPC :: CPU s Word16
getPC = getP pc

getIR :: CPU s (Maybe Interrupt)
getIR = getP ir

-- General cpu mods
mutP :: ASetter' Processor a -> (a -> a) -> CPU s ()
mutP = (%=)

mutA, mutX, mutY, mutSP, mutPS :: (Word8 -> Word8) -> CPU s ()
mutA = mutP a
mutX = mutP x
mutY = mutP y
mutSP = mutP sp
mutPS = mutP ps

mutPC :: (Word16 -> Word16) -> CPU s ()
mutPC = mutP pc

-- Some specialized combinators
setA, setX, setY, setSP, setPS :: Word8 -> CPU s ()
setA = mutA . const
setX = mutX . const
setY = mutY . const
setSP = mutSP . const
setPS = mutPS . const

setPC :: Word16 -> CPU s ()
setPC = mutPC . const

setIR' :: Interrupt -> CPU s ()
setIR' interrupt = ir ?= interrupt

-- Stack-specific operations
pushStack :: Word8 -> CPU s ()
pushStack v = do s <- getSP
                 writeRAM (0x0100 + to16 s) v
                 mutSP (\i -> i - 1)

popStack :: CPU s Word8
popStack = do mutSP (+1)
              s <- getSP
              readRAM (0x0100 + to16 s)

-- Get next instruction byte or arg at offset
eat8 :: CPU s Word8
eat8 = readPC >>= \r -> incPC >> return r
  where readPC = getPC >>= readRAM
        incPC = mutPC (+1)

eat16 :: CPU s Word16
eat16 = do b1 <- eat8
           b2 <- eat8
           return $ lendian b1 b2

-- Some flag helpers
-- Note: The lack of abstraction below is simply
--       because I could not be bothered.
stbit, clbit :: Bits a => Int -> a -> a
stbit = flip setBit
clbit = flip clearBit

-- Set flag
setN,setV,setB,setI,setZ,setC :: CPU s ()
setN = mutPS $ stbit 7
setV = mutPS $ stbit 6
setB = mutPS $ stbit 4
setI = mutPS $ stbit 2
setZ = mutPS $ stbit 1
setC = mutPS $ stbit 0

-- Clear flag
clrN, clrV, clrB, clrI, clrZ, clrC :: CPU s ()
clrN = mutPS $ clbit 7
clrV = mutPS $ clbit 6
clrB = mutPS $ clbit 4
clrI = mutPS $ clbit 2
clrZ = mutPS $ clbit 1
clrC = mutPS $ clbit 0

-- Inspect flag
sf :: Int -> CPU s Bool
sf i = flip testBit i <$> getPS

sN,sV,sB,sI,sZ,sC :: CPU s Bool
sN = sf 7
sV = sf 6
sB = sf 4
sI = sf 2
sZ = sf 1
sC = sf 0
