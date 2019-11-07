module Lib
    ( copyNIFTI
    ) where

import System.IO
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word

deserialiseHeader :: Get (Word32, Word32, Word32)
deserialiseHeader = do
  alen <- getWord32be
  plen <- getWord32be
  chksum <- getWord32be
  return (alen, plen, chksum)

getHandles :: IO (Handle, Handle)
getHandles = do
    -- TODO: make this parse cmd line args instead of prompting twice
    in_filename <- getLine
    out_filename <- getLine
    in_handle <- openBinaryFile in_filename ReadMode
    out_handle <- openBinaryFile out_filename WriteMode
    return (in_handle, out_handle)

copyNIFTI :: IO ()
copyNIFTI = do
    (inh, outh) <- getHandles
    rawData <- hGetContents inh -- read content lazily
    hPutStr outh rawData -- write content
    -- TODO: do processing here
    --close handles
    hClose inh
    hClose outh