module Main where

import Test.Hspec
import CPU
import Control.Monad
import Control.Applicative

main = do
  memorySpec
  flagSpec
  
-- CPU Tests
runOp' = fmap fst . (>>=) initCPU . runOp

-- Status flags spec tests
flagSpec = hspec $ do
  describe "Status Flag spec tests" $ do
    flagStatus

-- Memory spec tests
memorySpec = hspec $ do
  describe "Memory spec tests" $ do
    getSet
    zeroPageWrap
    indirect

getSet = do
  describe "Access and mutation" $ do
    it "zero page addressing" $ do
      runOp' getSetZ `shouldReturn` (0,5)
      
    it "absolute addressing" $ do
      runOp' getSetAddr `shouldReturn` (0,69)
  where getSetZ = do a <- getAddrZ 12
                     setAddrZ 12 5
                     getAddrZ 12 >>= return . (,) a
        getSetAddr = do a <- getAddr 345
                        setAddr 345 69
                        getAddr 345 >>= return . (,) a

zeroPageWrap = do
  describe "Zero page indexed addressing wrapping" $ do
    it "checks wrapping for Z,X addressing" $ do
      flip shouldReturn 12 $ runOp' $ setup X >> derefZ ZX 255

    it "checks wrapping for Z,Y addressing" $ do
      flip shouldReturn 12 $ runOp' $ setup Y >> derefZ ZY 255
  where setup reg = setReg reg 3 >> setAddrZ 2 12

indirect = do
  describe "Indirect addressing modes" $ do
    it "indexed-indirect addressing" $ do
      flip shouldReturn 67 $ runOp' $ do
        setAddrZ 7 0x03 >> setAddrZ 8 0x04
        setAddr 0x0403 67
        setReg X 0x02
        derefZ IXIN 0x05
    it "indirect-indexed addressing" $ do
      flip shouldReturn 67 $ runOp' $ do
        setReg Y 0x01
        setAddr 0x0403 67
        setAddrZ 7 0x02 >> setAddrZ 8 0x04
        derefZ INIX 0x07

-- Processor Status Flags
flagStatus = do
  describe "Set-Reset" $ do
    it "set flags" $ do
      flip shouldReturn True $ runOp' $
        allFlags (liftA2 (>>) setFlag checkFlag) (lift2Weird (&&)) True
    it "reset flags" $ do
      flip shouldReturn False $ runOp' $
        allFlags (liftA2 (>>) resetFlag checkFlag) (lift2Weird (&&)) True
  where allFlags :: Monad m => (Flag -> m a) -> (b -> a -> m b) -> b -> m b
        allFlags f m z = foldM m z =<< mapM (\fl -> f fl) [N ..]
        lift2Weird f = \a b -> return (a `f` b)
