{-# LANGUAGE RecordWildCards, BinaryLiterals, NegativeLiterals #-}

module CPU where

import Numeric
import Data.Char (intToDigit)
import Data.Int
import Data.Word
import Data.Bits
import Data.Array.IO
import qualified Data.Map.Strict as M
import Control.Monad.State
import Control.Monad.IO.Class

-- Helpers
-- | Promote 8-bit number t- 16-bit
to16 :: Integral a => a -> Word16
to16 = fromIntegral

-- | Truncate all bits > 8
to8 :: Integral a => a -> Word8
to8 = fromIntegral

iTo16 :: Int8 -> Word16
iTo16 = to16 . to8

-- | To signed 8-bit
toS8 :: Integral a => a -> Int8
toS8 = fromIntegral

type Memory = (ZeroPage,Mem)
type ZeroPage = IOUArray Word8 Int8
type Mem = IOUArray Word16 Int8

data CPU =
  CPU { _Mem :: Memory
      , _AReg :: Int8
      , _XReg :: Int8
      , _YReg :: Int8
      , _StackPtr :: Word8
      , _ProgCtr :: Word16
      , _Flags :: Word8 } deriving (Eq)

showCPU :: CPU -> IO ()
showCPU CPU {..} = do putStrLn $ "A-REG: " ++ hex _AReg
                      putStrLn $ "X-REG: " ++ hex _XReg
                      putStrLn $ "Y-REG: " ++ hex _YReg
                      putStrLn $ "STACK-PTR: " ++ hex _StackPtr
                      putStrLn $ "PROG-CNTR: " ++ hex' _ProgCtr
                      putStrLn $ "PROCESSOR STATUS: " ++ (showIntAtBase 2 intToDigit _Flags) ""
  where hex :: (Show a,Integral a) => a -> String
        hex = flip showHex "" . to8
        hex' = flip showHex ""

type Op = StateT CPU IO
runOp :: Op a -> CPU -> IO (a,CPU)
runOp op cpu = runStateT op cpu

-- * Flag Combinators
data Flag = N | V | D | I | Z | C deriving (Eq, Show, Ord, Enum)

getFlags :: Op Word8
getFlags = _Flags <$> get
              
setFlags :: Word8 -> Op ()
setFlags x = get >>= \cpu -> put (cpu { _Flags = x }) 

flagMap :: M.Map Flag Word8
flagMap = M.fromList $ [(N, 0b10000000)
                       ,(V, 0b01000000)
                       ,(D, 0b00001000)
                       ,(I, 0b00000100)
                       ,(Z, 0b00000010)
                       ,(C, 0b00000001)]

withFlags :: (Word8 -> Op a) -> Op a
withFlags f = _Flags <$> get >>= f

checkFlag :: Flag -> Op Bool
checkFlag f = let flag = flagMap M.! f
              in withFlags (\status -> return $ flag .&. status == flag)

modFlag :: Flag -> (Word8 -> Word8) -> Op ()
modFlag f fw = do cpu <- get
                  withFlags (\status -> put (cpu { _Flags = fw status }))

setFlag :: Flag -> Op ()
setFlag f = let flag = flagMap M.! f
            in modFlag f (.|. flag)

resetFlag :: Flag -> Op ()
resetFlag f = let flag = flagMap M.! f
              in modFlag f (.&. (complement flag))

boolFlag :: Flag -> Bool -> Op ()
boolFlag f b = if b then setFlag f else resetFlag f

-- Memory Helpers
getMemory :: Op Memory
getMemory = do cpu <- get
               return $ _Mem cpu

getAbs :: Op Mem
getAbs = snd <$> getMemory

getZPage :: Op ZeroPage
getZPage = fst <$> getMemory

-- REFACTOR
getA :: Word16 -> Op Int8
getA i = do mem <- getAbs
            liftIO $ readArray mem i 

getZ :: Word8 -> Op Int8
getZ i = do zpage <- getZPage
            liftIO $ readArray zpage i

setA :: Word16 -> Int8 -> Op ()
setA i v = do mem <- getAbs
              liftIO $ writeArray mem i v

setZ :: Word8 -> Int8 -> Op ()
setZ i v = do zpage <- getZPage
              liftIO $ writeArray zpage i v
-- REFACTOR

-- | Registers
data RegType = A | X | Y deriving (Eq, Show)

-- Helper
regAccess :: RegType -> (CPU -> Int8)
regAccess A = _AReg
regAccess X = _XReg
regAccess Y = _YReg

-- | Get value at register
getReg :: RegType -> Op Int8
getReg t = do cpu <- get
              return . regAccess t $ cpu

-- | Modify value at register
modReg :: RegType -> (Int8 -> Int8) -> Op ()
modReg t f = do cpu <- get
                let reg' = f . regAccess t $ cpu
                put $ case t of
                  A -> cpu { _AReg = reg' }
                  X -> cpu { _XReg = reg' }
                  Y -> cpu { _YReg = reg' }

-- | Set value of register to byte
setReg :: RegType -> Int8 -> Op ()
setReg t x = modReg t (const x)   

-- | Inrement or decrement register
incReg, decReg :: RegType -> Op ()
incReg t = modReg t (+1)
decReg t = modReg t (flip (-) 1)

-- * Program Counter and Stack Ptr
-- REFACTOR
getProgCtr :: Op Word16
getProgCtr = get >>= return . _ProgCtr
  
modProgCtr :: (Word16 -> Word16) -> Op ()
modProgCtr f = do cpu <- get
                  pCtr <- return . f . _ProgCtr $ cpu
                  put (cpu { _ProgCtr = pCtr })

getStackPtr :: Op Word8
getStackPtr = get >>= return . _StackPtr
modStackPtr :: (Word8 -> Word8) -> Op ()
modStackPtr f = do cpu <- get
                   sPtr <- return . f . _StackPtr $ cpu
                   put (cpu { _StackPtr = sPtr })

popStack :: Op Int8
popStack = do ptr <- to16 <$> getStackPtr
              modStackPtr (flip (-) 1)
              deref (AAbs AB ptr)

pushStack :: Int8 -> Op ()
pushStack x = do ptr <- to16 <$> getStackPtr
                 mutRef (AAbs AB (0x100 + ptr)) (const $ return x)
                 modStackPtr (+1)


-- REFACTOR
  
-- * Addressing memory

-- | Zero Page Addressing
data ZeroMode =
  ZR | ZX | ZY | IXIN | INIX
  deriving (Eq, Show)

-- |Other addressing modes
data AbsMode =
  AB | AX | AY
  deriving (Eq, Show)

-- | Dereference ZeroPage address
derefZ :: ZeroMode -> Word8 -> Op Int8
derefZ ZR addr = getZ addr
derefZ ZX addr = getReg X >>= getZ . (addr+) . to8
derefZ ZY addr = getReg Y >>= getZ . (addr+) . to8
derefZ IXIN addr = do addr' <- getReg X >>= return . (addr +) . to8
                      let addr'' = addr' + 1
                      (x,y) <- (,) <$> getZ addr' <*> getZ addr''
                      getA $ littleEndian (to8 x) (to8 y)
derefZ INIX addr = do (x,y) <- (,) <$> getZ addr <*> getZ (addr+1)
                      getReg Y >>= \y' -> getA
                        $ to16 y' + littleEndian (to8 x) (to8 y)
-- Helper
littleEndian :: Word8 -> Word8 -> Word16
littleEndian lo hi = let (hi',lo') = (shiftL (to16 hi) 8,to16 lo)
                     in hi' + lo'
                       
--

-- | Dereference Address
derefA :: AbsMode -> Word16 -> Op Int8
derefA AB addr = getA addr
derefA AX addr = getReg X >>= getA . add8 addr . to8
derefA AY addr = getReg Y >>= getA . add8 addr . to8

-- Helper
add8 :: Word16 -> Word8 -> Word16
add8 x = (+x) . fromIntegral
--

-- General Memory
data Address = AZero ZeroMode Word8
             | AAbs  AbsMode  Word16
             | AImm Word8 -- Can maybe get rid of this during parsing
             | AAcc
             deriving (Eq, Show)

-- | Dereference Memory
deref :: Address -> Op Int8
deref (AZero zm addr) = derefZ zm addr
deref (AAbs  am addr) = derefA am addr
deref (AImm val)      = return . toS8 $ val
deref AAcc            = getReg A >>= return . toS8

-- | Reference memory for storage
mutRef :: Address -> (Int8 -> Op Int8) -> Op ()
mutRef (AImm _) _ = error "Cannot reference literal value"
mutRef AAcc     f = getReg X >>= return. const () . f
mutRef addr f = do val <- deref addr
                   case addr of
                     (AZero _ a) -> f val >>= setZ a
                     (AAbs  _ a) -> f val >>= setA a

mutRef' :: Address -> (Int8 -> Int8) -> Op ()
mutRef' addr f = mutRef addr (return . f)

-- | Modify combinator, dereferences and modifies memory


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
                     , _ProgCtr = 0
                     , _Flags = 0b00000000 }
  where initZ = newArray (0,255) 0
        initMem = newArray (256,65535) 0

testOp :: Show a => Op a -> IO ()
testOp op = do cpu <- initCPU
               (a,cpu') <- runOp op cpu
               putStrLn $ "Output: " ++ show a
               showCPU cpu'
