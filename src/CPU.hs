{-# LANGUAGE RecordWildCards #-}

module CPU where

import Numeric
import Data.Word
import Data.Bits
import Data.Array.IO
import Control.Monad.State
import Control.Monad.IO.Class

-- Helpers
to16 :: Word8 -> Word16
to16 = fromIntegral

type Memory = (ZeroPage,Mem)
type ZeroPage = IOUArray Word8 Word8
type Mem = IOUArray Word16 Word8

data CPU =
  CPU { _Mem :: Memory
      , _AReg :: Word8
      , _XReg :: Word8
      , _YReg :: Word8
      , _StackPtr :: Word8
      , _ProgCntr :: Word8 } deriving (Eq)

showCPU :: CPU -> IO ()
showCPU CPU {..} = do putStrLn $ "A-REG: " ++ show _AReg
                      putStrLn $ "X-REG: " ++ show _XReg
                      putStrLn $ "Y-REG: " ++ show _YReg
                      putStrLn $ "STACK-PTR: " ++ show _StackPtr
                      putStrLn $ "PROG-CNTR: " ++ show _ProgCntr

type Op = StateT CPU IO
runOp :: Op a -> CPU -> IO (a,CPU)
runOp op cpu = runStateT op cpu

-- Memory Helpers
getMemory :: Op Memory
getMemory = do cpu <- get
               return $ _Mem cpu

getMem :: Op Mem
getMem = snd <$> getMemory

getZ :: Op ZeroPage
getZ = fst <$> getMemory

-- REFACTOR
getAddr :: Word16 -> Op Word8
getAddr i = do mem <- getMem
               liftIO $ readArray mem i 

getAddrZ :: Word8 -> Op Word8
getAddrZ i = do zpage <- getZ
                liftIO $ readArray zpage i

setAddr :: Word16 -> Word8 -> Op ()
setAddr i v = do mem <- getMem
                 liftIO $ writeArray mem i v

setAddrZ :: Word8 -> Word8 -> Op ()
setAddrZ i v = do zpage <- getZ
                  liftIO $ writeArray zpage i v
-- REFACTOR

-- | Registers
data RegType = A | X | Y | S | P deriving (Eq, Show)

-- Helper
regAccess :: RegType -> (CPU -> Word8)
regAccess A = _AReg
regAccess X = _XReg
regAccess Y = _YReg
regAccess S = _StackPtr
regAccess P = _ProgCntr

-- | Get value at register
getReg :: RegType -> Op Word8
getReg t = do cpu <- get
              return . regAccess t $ cpu

-- | Modify value at register
modReg :: RegType -> (Word8 -> Word8) -> Op ()
modReg t f = do cpu <- get
                let reg' = f . regAccess t $ cpu
                put $ case t of
                  A -> cpu { _AReg = reg' }
                  X -> cpu { _XReg = reg' }
                  Y -> cpu { _YReg = reg' }
                  S -> cpu { _StackPtr = reg' }
                  P -> cpu { _ProgCntr = reg' }

-- | Set value of register to byte
setReg :: RegType -> Word8 -> Op ()
setReg t x = modReg t (const x)   

-- | Inrement or decrement register
incReg, decReg :: RegType -> Op ()
incReg t = modReg t (+1)
decReg t = modReg t (flip (-) 1)
 
-- ^ Addressing memory

-- | Zero Page Addressing
data ZeroAddr =
  ZR | ZX | ZY | IXIN | INIX
  deriving (Eq, Show)

-- |Other addressing modes
data Addr =
  AB | AX | AY
  deriving (Eq, Show)

-- | Dereference ZeroPage address
derefZ :: ZeroAddr -> Word8 -> Op Word8
derefZ ZR addr = getAddrZ addr
derefZ ZX addr = getReg X >>= getAddrZ . (addr+)
derefZ ZY addr = getReg Y >>= getAddrZ . (addr+)
derefZ IXIN addr = do addr' <- getReg X >>= return . (+addr)
                      let addr'' = addr' + 1
                      (x,y) <- (,) <$> getAddrZ addr' <*> getAddrZ addr''
                      getAddr $ littleEndian x y
derefZ INIX addr = do (x,y) <- (,) <$> getAddrZ addr <*> getAddrZ (addr+1)
                      getReg Y >>= \y' -> getAddr
                        $ to16 y' + littleEndian x y
-- Helper
littleEndian :: Word8 -> Word8 -> Word16
littleEndian lo hi = let (hi',lo') = (shiftL (to16 hi) 8,to16 lo)
                     in hi' + lo'
                       
--

-- | Dereference Address
derefAddr :: Addr -> Word16 -> Op Word8
derefAddr AB addr = getAddr addr
derefAddr AX addr = getReg X >>= getAddr . add8 addr
derefAddr AY addr = getReg Y >>= getAddr . add8 addr

-- Helper
add8 :: Word16 -> Word8 -> Word16
add8 x = (+x) . fromIntegral
--

-- Doing work
-- Initialize the CPU
initCPU :: IO CPU
initCPU = do mem <- initMem
             z <- initZ
             return
               $ CPU { _Mem = (z,mem)
                     , _AReg = 0
                     , _XReg = 0
                     , _YReg = 0
                     , _StackPtr = 0
                     , _ProgCntr = 0 }
  where initZ = newArray (0,255) 0
        initMem = newArray (256,65535) 0