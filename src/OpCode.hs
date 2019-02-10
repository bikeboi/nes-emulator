{-# LANGUAGE NegativeLiterals, ScopedTypeVariables #-}

module OpCode where

import Prelude hiding (and)

import CPU
import Data.Word
import Data.Int
import Data.Bits
import Control.Monad

-- | Add with Carry
adc :: Int8 -> Op ()
adc y = do a <- getReg A
           c <- carryVal
           let y' = c + y
               z = a + y'
           carryAdd a y'
           pure z >>= zero >>= neg
           ovf a y' Add
           setReg A z

-- | Subtract with Carry
sbc :: Int8 -> Op ()
sbc y = do a <- getReg A
           c <- (-) 1 <$> carryVal
           let y' = y - c
               z = a - y'
               ovfCond = checkOverflow a y' Sub
           boolFlag C (not ovfCond)
           pure z >>= zero >>= neg
           boolFlag V ovfCond
           setReg A z

-- Incrementing and decrementing
data IncDec = Inc | Dec deriving (Eq, Show)
procRes :: Either RegType Address -> Int8 -> Op ()
procRes (Left reg) x = zero x >>= neg >>= setReg reg 
procRes (Right a)  x = mutRef a $ \val -> zero x >>= neg

incDec :: Either RegType Address -> IncDec -> Op ()
incDec x@(Left reg) incdc = do r <- getReg reg
                               procRes x $ case incdc of
                                 Inc -> r + 1
                                 Dec -> r - 1
incDec x@(Right a)  incdc = do val <- deref a
                               procRes x $ case incdc of
                                             Inc -> val + 1
                                             Dec -> val - 1

-- | Decrement memory location
dec :: Address -> Op ()
dec addr = incDec (Right addr) Dec

-- | Decrement X register
dex :: Op ()
dex = incDec (Left X) Dec

-- | Decrement Y register
dey :: Op ()
dey = incDec (Left Y) Dec

-- | Increment Memory
inc :: Address -> Op ()
inc addr = incDec (Right addr) Inc

-- | Increment X register
inx :: Op ()
inx = incDec (Left X) Inc

-- | Increment Y register
iny :: Op ()
iny = incDec (Left Y) Inc

-- | Arithmetic Shift Left
asl :: Address -> Op ()
asl addr = mutRef addr $ \v -> do
  let res = shiftL v 1
  boolFlag C (testBit v 7)
  zero res >>= neg

-- | XOR
eor :: Address -> Op ()
eor addr = do a <- getReg A
              val <- deref addr
              let res = val `xor` a
              zero res >>= neg >>= setReg A

-- | Logical Shift Right
lsr :: Address -> Op ()
lsr addr = mutRef addr $ \val -> do
  boolFlag C (testBit val 0)
  pure (shiftR val 1) >>= zero >>= neg

-- | Rotate Left


-- | Logical AND
and :: Int8 -> Op ()
and val = mutRef AAcc $ \a -> do
  let res = a .&. val
  zero res >>= neg

-- | Logical inclusive OR
ora :: Address -> Op ()
ora addr = do val <- deref addr
              a <- getReg A
              zero (val .|. a) >>= neg >>= setReg A

-- Branch helper
branchIf :: Op Bool -> Int8 -> Op ()
branchIf mb x = do b <- mb
                   if b then modProgCtr (iTo16 x +)
                     else return ()

-- Branch Operations
bcc,bcs,beq,bmi,bne,bpl,bvc,bvs :: Int8 -> Op ()
bcc = branchIf $ checkFlag C
bcs = branchIf $ not <$> checkFlag C
beq = branchIf $ checkFlag Z
bmi = branchIf $ checkFlag N
bne = branchIf $ not <$> checkFlag Z
bpl = branchIf $ not <$> checkFlag N
bvc = branchIf $ not <$> checkFlag V
bvs = branchIf $ checkFlag V

-- Jumps
jmp :: Word16 -> Op ()
jmp address = modProgCtr (const address)

jsr :: Word16 -> Op ()
jsr = undefined

-- Clearing flags
clc, cld, cli, clv :: Op ()
clc = resetFlag C
cld = resetFlag D
cli = resetFlag I
clv = resetFlag V

-- No Operation
nop :: Op ()
nop = return ()

-- Comparisons
comparison :: Op Int8 -> Int8 -> Op ()
comparison ma x = do val <- ma
                     boolFlag C (val >= x)
                     boolFlag Z (val == x)
                     boolFlag N (val < x)

cmp, cpx, cpy :: Int8 -> Op ()
cmp = comparison $ getReg A
cpx = comparison $ getReg X
cpy = comparison $ getReg Y

-- Loading
ld :: RegType -> Address -> Op ()
ld r addr = deref addr >>= zero >>= neg >>= setReg r

lda, ldx, ldy :: Address -> Op ()
lda = ld A
ldx = ld X
ldy = ld Y

-- Stack operations
pha :: Op ()
pha = do a <- getReg A
         pushStack $ toS8 a

php :: Op ()
php = do status <- getFlags
         pushStack $ toS8 status

pla :: Op ()
pla = popStack >>= zero >>= neg >>= setReg A

plp :: Op ()
plp = popStack >>= setFlags . to8 

--
-- HELPERS
-- Combinators for activating flags
carryAdd :: Int8 -> Int8 -> Op ()
carryAdd x y = let (x',y') = (to8 x, to8 y)
            in boolFlag C $ to16 x' + to16 y' > 255

zero :: Int8 -> Op Int8
zero x = do boolFlag Z $ x == 0
            return x

neg :: Int8 -> Op Int8
neg x = do boolFlag N $ x < 0
           return x
  
ovf :: Int8 -> Int8 -> AddOrSub -> Op ()
ovf a b aos = boolFlag V (checkOverflow a b aos)

-- Flag condition checking
-- Check overflow
data AddOrSub = Add | Sub deriving (Eq, Show)
diffSign :: Int8 -> Int8 -> Bool
diffSign x y = x `xor` y < 0

checkOverflow :: Int8 -> Int8 -> AddOrSub -> Bool
checkOverflow x y Add = checkAdd x y
checkOverflow x y Sub = checkSub x y

checkAdd :: Int8 -> Int8 -> Bool
checkAdd x y = case diffSign x y of
                 True -> False
                 False -> if x > 0 -- If both negative | If both positive
                          then 127 - x < y
                          else -128 - x > y
checkSub :: Int8 -> Int8 -> Bool
checkSub x y = diffSign x y && checkAdd x (negate y)

-- Carry value heloer
carryVal :: Op Int8
carryVal = do c <- checkFlag C
              return $
                if c then 1 else 0

