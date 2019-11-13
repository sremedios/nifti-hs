module Endianness where

import Data.Word
import Data.Int
import qualified Data.Binary.Get as G

data Endianness
  = Little
  | Big
  | Host

getWord8 :: G.Get Word8
getWord8 = G.getWord8

getWord16 :: Endianness -> G.Get Word16
getWord16 Little = G.getWord16le
getWord16 Big = G.getWord16be
getWord16 Host = G.getWord16host

getWord32 :: Endianness -> G.Get Word32
getWord32 Little = G.getWord32le
getWord32 Big = G.getWord32be
getWord32 Host = G.getWord32host

getWord64 :: Endianness -> G.Get Word64
getWord64 Little = G.getWord64le
getWord64 Big = G.getWord64be
getWord64 Host = G.getWord64host

getInt32 :: Endianness -> G.Get Int32
getInt32 Little = G.getInt32le
getInt32 Big = G.getInt32be
getInt32 Host = G.getInt32host

getFloat :: Endianness -> G.Get Float
getFloat Little = G.getFloatle
getFloat Big = G.getFloatbe
getFloat Host = G.getFloathost
