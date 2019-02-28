{-# LANGUAGE RankNTypes, RecordWildCards #-}

module CPU.Internal (
  Processor
  , CPU, runCPU, procc
  , readRAM, writeRAM
  , zp, zpX, zpY, ab, abX, abY
  , ind, inIx, ixIn, rel
  , regA, regX, regY, stack, status, prog
  , mutA, mutX, mutY, mutStack, mutStatus, mutProg
  , setA, setX, setY, setStack, setStatus, setProg
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

import Data.Array.MArray
import Control.Monad.STM
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Class (lift)

-- Data Types
data Processor =
  Processor { _status :: Word8
            , _regA :: Word8
            , _regX :: Word8
            , _regY :: Word8
            , _stack :: Word8
            , _prog :: Word16 } deriving Eq

instance Show Processor where
  show Processor{..} = mconcat . map (\(a,b) -> a ++ b ++ " ") $
                       [("A:", show16 _regA)
                       ,("X:", show16 _regX)
                       ,("Y:", show16 _regY)
                       ,("PC:", showBig16 _prog)
                       ,("SP:", show16 _stack)
                       ,("STAT: ", show2 _status)]

initProcessor :: Processor
initProcessor =
  Processor { _status = 0x00
            , _regA   = 0x00
            , _regX   = 0x00
            , _regY   = 0x00
            , _stack  = 0xFD
            , _prog   = 0x6000 }

-- ROM
loadROM :: [Word8] -> CPU ()
loadROM bs = zipWithM writeRAM [0x6000..] bs >> return ()

modStatus, modA, modX, modY, modStack :: (Word8 -> Word8) -> Processor -> Processor
modStatus f p@Processor{..} = p { _status = f _status }
modA f p@Processor{..} = p { _regA = f _regA }
modX f p@Processor{..} = p { _regX = f _regX }
modY f p@Processor{..} = p { _regY = f _regY }
modStack f p@Processor{..} = p { _stack = f _stack }
modProg :: (Word16 -> Word16) -> Processor -> Processor
modProg f p@Processor{..} = p { _prog = f _prog }

-- The CPU Monad
type CPUT m arr = ExceptT String (ReaderT arr (StateT Processor m))
type CPU = CPUT MemMonad Memory

runCPU :: (Monad m, MArray a e m, Ix i) => a i e -> CPUT m (a i e) x -> m (Either String x,Processor)
runCPU mem op = runStateT (runReaderT (runExceptT op) mem) initProcessor

-- Combinators Galore
procc :: CPU Processor
procc = get

-- Memory
readRAM :: Word16 -> CPU Word8
readRAM a = do mem <- ask
               lift . lift . lift $ readMem mem a

writeRAM :: Word16 -> Word8 -> CPU ()
writeRAM a v = do mem <- ask
                  lift . lift . lift $ writeMem mem a v

-- General CPU access
status, regX, regY, regA, stack :: CPU Word8
status = _status <$> procc
regA   = _regA <$> procc
regX   = _regX <$> procc
regY   = _regY <$> procc
stack  = _stack <$> procc

prog :: CPU Word16
prog = _prog <$> procc

-- General cpu mods
mutReg :: ((a -> a) -> Processor -> Processor) -> (a -> a) -> CPU ()
mutReg modf f = modify (\pr -> modf f pr)

mutA, mutX, mutY, mutStack, mutStatus :: (Word8 -> Word8) -> CPU ()
mutA = mutReg modA
mutX = mutReg modX
mutY = mutReg modY
mutStack = mutReg modStack
mutStatus = mutReg modStatus

mutProg :: (Word16 -> Word16) -> CPU ()
mutProg = mutReg modProg     

-- Some specialized combinators
setA, setX, setY, setStack, setStatus :: Word8 -> CPU ()
setA = mutA . const
setX = mutX . const
setY = mutY . const
setStack = mutStack . const
setStatus = mutStatus . const

setProg :: Word16 -> CPU ()
setProg = mutProg . const

-- Stack-specific operations
pushStack :: Word8 -> CPU ()
pushStack v = do s <- stack
                 writeRAM (0x0100 + to16 s) v
                 mutStack (\i -> i - 1)

popStack :: CPU Word8
popStack = do mutStack (+1)
              s <- stack
              readRAM (0x0100 + to16 s)
-- Program counter
readProg :: CPU Word8
readProg = prog >>= readRAM

nextProg :: CPU ()
nextProg = mutProg (+1)

-- Get next instruction byte or arg at offset
eat8 :: CPU Word8
eat8 = readProg >>= \r -> nextProg >> return r

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
setN = mutStatus $ stbit 7
setV = mutStatus $ stbit 6
setB = mutStatus $ stbit 4
setI = mutStatus $ stbit 2
setZ = mutStatus $ stbit 1
setC = mutStatus $ stbit 0

-- Clear flag
clrN, clrV, clrB, clrI, clrZ, clrC :: CPU ()
clrN = mutStatus $ clbit 7
clrV = mutStatus $ clbit 6
clrB = mutStatus $ clbit 4
clrI = mutStatus $ clbit 2
clrZ = mutStatus $ clbit 1
clrC = mutStatus $ clbit 0

-- Inspect flag
sf :: Int -> CPU Bool
sf i = flip testBit i <$> status

sN,sV,sB,sI,sZ,sC :: CPU Bool
sN = sf 7
sV = sf 6
sB = sf 4
sI = sf 2
sZ = sf 1
sC = sf 0

-- Addressing Modes
zp :: Word8 -> CPU Word16
zp = return . to16

zpIx :: Word8 -> Word8 -> CPU Word16
zpIx x ix = return . to16 $ x + ix

zpX, zpY :: Word8 -> CPU Word16
zpX x = regX >>= zpIx x
zpY x = regY >>= zpIx x

rel :: Int8 -> CPU Word16
rel a = do p <- prog
           let lo = toS8 p + a
           return $ (p .&. 0xFF00) + to16 lo

ab :: Word16 -> CPU Word16
ab = return

abIx :: Word16 -> Word8 -> CPU Word16
abIx a ix = return $ a + (to16 ix)

abX, abY :: Word16 -> CPU Word16
abX x = regX >>= abIx x
abY x = regY >>= abIx x

ind :: Word16 -> CPU Word16
ind a = do let a' = (a .&. 0xFF00) + to16 (low a + 1)
           b1 <- readRAM a
           b2 <- readRAM a'
           return $ lendian b1 b2

ixIn :: Word8 -> CPU Word16
ixIn a = do x <- regX
            ind' . to16 $ a + x
  where ind' addr = do b1 <- readRAM addr
                       b2 <- readRAM $ addr+1
                       return $ lendian b1 b2
            
inIx :: Word8 -> CPU Word16
inIx a = ind (to16 a) >>= \aa -> fmap (\r -> to16 r + aa) regY 
