{-# LANGUAGE RecordWildCards #-}

module CPU.ROM where

import Util
import Memory
import CPU.Internal

import Data.Word
import Data.Bits (testBit)
import qualified Data.ByteString as B
import Control.Monad.Except (liftEither)

-- ROM LOADER
data ROMSpec =
  ROMSpec { _trainer :: Bool
          , _mapper  :: Mapper
          , _prg     :: Word8
          , _chr     :: Word8
          } deriving (Eq, Show)

data Mapper = NROM deriving (Eq, Show)

calcMapper :: Word8 -> Word8 -> Either String Mapper
calcMapper 0 0 = Right NROM
calcMapper x y = Left $ "Unsupported Mapper: " ++ show16 x ++ show16 y

loadROM :: B.ByteString -> CPU s ()
loadROM b = let (header,body) = B.splitAt 16 b
            in do spec <- liftEither $ parseSpec header
                  loadSpec spec body

parseSpec :: B.ByteString -> Either String ROMSpec
parseSpec b = do b' <- checkNES b
                 ROMSpec
                   <$> train b
                   <*> mapper b
                   <*> prg b
                   <*> chr b
  where train  bs = pure $ testBit f6 2
        mapper bs = calcMapper f6 f7
        prg    bs = pure $ B.index bs 4
        chr    bs = pure $ B.index bs 5
        checkNES bs = case (B.length bs == 16,checkConst bs) of
                        (True,True) -> Right bs
                        (False,_)   -> Left "NES Header too short"
                        (_,False)   -> Left "Invalid NES Header"
        checkConst b = B.unpack (B.take 4 b) == [0x4E,0x45,0x53,0x1A]
        (f6,f7) = (,) <$> flip B.index 6 <*> flip B.index 7 $ b

loadSpec :: ROMSpec -> B.ByteString -> CPU s ()
loadSpec spec@ROMSpec{..} b =
  let start = if _trainer then 512 else 0
  in case _mapper of
       NROM -> loadNROM spec (B.drop start b)

loadNROM :: ROMSpec -> B.ByteString -> CPU s ()
loadNROM ROMSpec{..} bs =
  let (lower,upper) = B.splitAt (1024*16) bs
      bankSize = 1024 * 16
  in do loadChunkRAM lower 0x8000
        case _prg > 1 of
          False -> loadChunkRAM lower 0xC000
          True  -> loadChunkRAM (B.take bankSize upper) 0xC000
