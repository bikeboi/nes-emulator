{-# LANGUAGE GeneralizedNewtypeDeriving
             , TemplateHaskell
             , RecordWildCards
             , BinaryLiterals
             , RankNTypes #-}

module CPU.Internal (
  Processor, Interrupt(..)
  , CPU, runCPU, proccessor, cpuErr
  , readRAM, writeRAM, loadChunkRAM
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

-- The CPU Monad
newtype CPU a =
  CPU { unCPU :: ExceptT String (ReaderT Memory (StateT Processor IO)) a }
  deriving (Functor, Applicative, Monad
           , MonadError String
           , MonadReader Memory
           , MonadState Processor
           , MonadIO)

runCPU :: Memory -> CPU a -> IO (Either String a,Processor)
runCPU mem op = runStateT (runReaderT (runExceptT  (unCPU op)) mem) initProcessor

cpuErr :: String -> CPU a
cpuErr = throwError

-- Combinators Galore
proccessor :: CPU Processor
proccessor = get

-- Interrupts
setIR :: Interrupt -> CPU ()
setIR NMI = setIR NMI
setIR ir = do i <- sI
              if i then return () else setIR ir

-- Memory
readRAM :: Word16 -> CPU Word8
readRAM a = do mem <- ask
               liftIO $ readMem mem a

writeRAM :: Word16 -> Word8 -> CPU ()
writeRAM a v = do mem <- ask
                  liftIO $ writeMem mem a v

loadChunkRAM :: B.ByteString -> Word16 -> CPU ()
loadChunkRAM b s = do mem <- ask
                      liftIO $ loadChunk b s mem

-- General CPU access
getP :: SimpleGetter Processor a -> CPU a
getP s = view s <$> proccessor

getPS, getX, getY, getA, getSP :: CPU Word8
getPS  = getP ps
getA   = getP a
getX   = getP x
getY   = getP y
getSP  = getP sp

getPC :: CPU Word16
getPC = getP pc

getIR :: CPU (Maybe Interrupt)
getIR = getP ir

-- General cpu mods
mutP :: ASetter' Processor a -> (a -> a) -> CPU ()
mutP = (%=)

mutA, mutX, mutY, mutSP, mutPS :: (Word8 -> Word8) -> CPU ()
mutA = mutP a
mutX = mutP x
mutY = mutP y
mutSP = mutP sp
mutPS = mutP ps

mutPC :: (Word16 -> Word16) -> CPU ()
mutPC = mutP pc

-- Some specialized combinators
setA, setX, setY, setSP, setPS :: Word8 -> CPU ()
setA = mutA . const
setX = mutX . const
setY = mutY . const
setSP = mutSP . const
setPS = mutPS . const

setPC :: Word16 -> CPU ()
setPC = mutPC . const

setIR' :: Interrupt -> CPU ()
setIR' interrupt = ir ?= interrupt

-- Stack-specific operations
pushStack :: Word8 -> CPU ()
pushStack v = do s <- getSP
                 writeRAM (0x0100 + to16 s) v
                 mutSP (\i -> i - 1)

popStack :: CPU Word8
popStack = do mutSP (+1)
              s <- getSP
              readRAM (0x0100 + to16 s)

-- Get next instruction byte or arg at offset
eat8 :: CPU Word8
eat8 = readPC >>= \r -> incPC >> return r
  where readPC = getPC >>= readRAM
        incPC = mutPC (+1)

eat16 :: CPU Word16
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
setN,setV,setB,setI,setZ,setC :: CPU ()
setN = mutPS $ stbit 7
setV = mutPS $ stbit 6
setB = mutPS $ stbit 4
setI = mutPS $ stbit 2
setZ = mutPS $ stbit 1
setC = mutPS $ stbit 0

-- Clear flag
clrN, clrV, clrB, clrI, clrZ, clrC :: CPU ()
clrN = mutPS $ clbit 7
clrV = mutPS $ clbit 6
clrB = mutPS $ clbit 4
clrI = mutPS $ clbit 2
clrZ = mutPS $ clbit 1
clrC = mutPS $ clbit 0

-- Inspect flag
sf :: Int -> CPU Bool
sf i = flip testBit i <$> getPS

sN,sV,sB,sI,sZ,sC :: CPU Bool
sN = sf 7
sV = sf 6
sB = sf 4
sI = sf 2
sZ = sf 1
sC = sf 0
